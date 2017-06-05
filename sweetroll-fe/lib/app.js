'use strict'

const helpers = require('./helpers')
const URI = require('urijs')
const LinkHeader = require('http-link-header')
const moment = require('moment')
const fetch = require('node-fetch')
const webpush = require('web-push')
const qs = require('qs')
const Retry = require('promised-retry')
const _ = require('lodash')
const { head, merge, concat, get, isObject, groupBy, includes, isString, debounce, some } = _
const pify = require('pify')
const pug = require('pug')
const pugTryCatch = require('pug-plugin-try-catch')
const pg = require('pg')
const { default: PgAsync, SQL } = require('pg-async')

const env = process.env
const assets = require('dynamic-asset-rev')('dist')
const log = require('debug')('sweetroll-fe')
const sse = require('sse-broadcast')({ compression: true })
const relaxCSP = env.RELAX_CSP // Mainly for micro-panel development
const isProxied = env.IS_PROXIED // Whether to trust X-Forwarded-Proto
const cacheTemplates = env.CACHE_TEMPLATES // Do not recompile templates on every request. Enable in prod for perf boost
const cache = env.DO_CACHE ? require('lru-cache')({
	max: parseInt(env.CACHE_MAX_ITEMS || '128')
}) : null
const websubHub = env.WEBSUB_HUB || 'https://switchboard.p3k.io'
const websubHubMode = env.WEBSUB_HUB_MODE || 'multi' // Whether to do one request for all URLs affected by a change or one request per URL
const indieAuthEndpoint = env.INDIEAUTH_ENDPOINT || 'https://indieauth.com/auth'
const mediaEndpoint = env.MEDIA_ENDPOINT || '/micropub/media'
const microPanelRoot = env.MICRO_PANEL_ROOT || '/dist/micro-panel' // To allow using the unpacked version of micro-panel in development
const webmentionOutbox = env.WEBMENTION_OUTBOX // Allow using an external sender like Telegraph, default to sending on our own
const webmentionOutboxConf = JSON.parse(env.WEBMENTION_OUTBOX_CONF || '{}') // Something like {token: '...'} for Telegraph
const allowedCdns = env.ALLOWED_CDNS || '' // List of allowed CDN domains for Content-Security-Policy
const vapidKeys = { publicKey: env.VAPID_PUBLIC_KEY, privateKey: env.VAPID_PRIVATE_KEY, contact: env.VAPID_CONTACT } // Web Push keypair
if (vapidKeys.publicKey) {
	webpush.setVapidDetails(vapidKeys.contact, vapidKeys.publicKey, vapidKeys.privateKey)
}
const dbsettings = require('pg-connection-string').parse(env.DATABASE_URL || env.DATABASE_URI || 'postgres://localhost/sweetroll')
dbsettings.application_name = 'sweetroll-fe'
dbsettings.max = parseInt(env.PG_POOL_MAX || '4')
dbsettings.ssl = env.PG_SSL
const db = new PgAsync(dbsettings)

const common = require('../../sweetroll-node-common')
const addAuth = common.authentication(log, require('jsonwebtoken').verify)

const render = async (file, tplctx) =>
	pify(pug.renderFile)('views/' + file, tplctx)

const affectedUrls = async (url) => {
	const norm = new URI(url).normalizePort().normalizeHostname()
	const objUriStr = norm.clone().toString()
	const domainUriStr = norm.pathname('').search('').toString()
	const { obj, feeds } = await db.row(SQL`SELECT
		mf2.objects_smart_fetch(${objUriStr}, ${domainUriStr}, 1, null, null, null) AS obj,
		mf2.objects_fetch_feeds(${domainUriStr}) AS feeds
	`)
	if (!obj) return [url]
	if (!feeds) return [url]
	return {
		affUrls: concat(helpers.matchingFeeds(feeds, obj).map(x => x.url), url),
		obj,
		feeds,
		domainUriStr
	}
}

const findWebmentionEndpoint = async (target) => {
	const resp = await fetch(target, {
		headers: {
			'Accept': 'text/html',
			'Accept-Charset': 'utf-8',
			'User-Agent': 'Sweetroll',
		},
		redirect: 'follow',
		timeout: 15 * 1000,
	})
	const lh = LinkHeader.parse(resp.headers.get('Link') || '')
	let result = head(lh.get('rel', 'webmention').map(l => l.uri)) ||
		head(helpers.getHtmlLinksByRel(await resp.text()))
	if (isString(result)) {
		result = new URI(result).absoluteTo(resp.url).toString()
	}
	return result
}

let localDomains = []

const updateLocalDomains = debounce(() => {
	db.rows(SQL`SELECT properties->'url'->>0 AS url FROM mf2.objects WHERE properties->'site-settings' IS NOT NULL`).then(rows => {
		localDomains = rows.map(x => x.url)
	})
}, 2000)

updateLocalDomains()

const onWebmention = async ({ source, target }) => {
	const domainUriStr = new URI(target).normalizePort().normalizeHostname().pathname('').search('').toString()
	const { dobj } = await db.row(SQL`SELECT mf2.objects_smart_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj`)

	// Web Push notifications
	for (const subscription of get(dobj, 'properties.site-web-push-subscriptions', [])) {
		try {
			await webpush.sendNotification(subscription, JSON.stringify({
				url: source,
				body: 'Got response ' + source + ' to ' + target
			}))
		} catch (err) {
			log('Could not send push notification: %O', err)
		}
	}
}

const onEntryChange = async (eventUrl) => {
	const { affUrls, obj, domainUriStr } = await affectedUrls(eventUrl)
	const mentions = helpers.findMentionedLinks(obj)
	log('got change notification for %s, affected URLs: %o, mentioned URLs: %o', eventUrl, affUrls, mentions)

	// Cache invalidation
	if (cache) {
		for (const u of affUrls) {
			log('cache delete: %s', u)
			cache.del(u)
		}
	}

	// Publishing to /live
	const grouped = groupBy(affUrls, u => new URI(u).host())
	for (const domain of Object.keys(grouped)) {
		sse.publish(domain, 'change', grouped[domain].join(','))
	}

	// Publishing to WebSub
	async function pushWebSub (urls) {
		try {
			const resp = await fetch(websubHub, {
				method: 'POST',
				headers: { 'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8' },
				body: qs.stringify({ 'hub.mode': 'publish', 'hub.topic': urls }, { arrayFormat: 'brackets' }),
			})
			log('WebSub hub response: %s %s %O', resp.status, resp.statusText, await resp.text())
		} catch (err) {
			log('Could not ping the WebSub hub: %O', err)
		}
	}
	if (websubHubMode === 'multi') {
		pushWebSub(affUrls)
	} else {
		for (const url of affUrls) {
			pushWebSub(url)
		}
	}

	// Sending Webmentions
	for (const target of mentions) {
		try {
			const endp = webmentionOutbox || await findWebmentionEndpoint(target)
			if (!endp) {
				log('No Webmention endpoint found for %o', target)
				continue
			}
			const resp = await fetch(endp, {
				method: 'POST',
				headers: { 'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8' },
				body: qs.stringify(merge(webmentionOutboxConf, { source: eventUrl, target })),
				redirect: 'follow',
				timeout: 40 * 1000,
			})
			log('Webmention endpoint response for %o: %s %s %s', target, resp.status, resp.statusText, await resp.text())
		} catch (err) {
			log('Could not send Webmention to %o: %O', target, err)
		}
	}
}

const notificationListener = new Retry({
	name: 'notificationListener',
	log: require('debug')('retry:pg-listen'),
	try () {
		const ldb = new pg.Client(dbsettings)
		ldb.on('error', () => {
			notificationListener.reset()
			notificationListener.try()
		})
		return new Promise((resolve, reject) => {
			ldb.connect(err => { err ? reject(err) : resolve(ldb) })
		})
	},
	success (ldb) {
		ldb.on('notification', msg => {
			try {
				const payload = JSON.parse(msg.payload)
				const eventUrl = payload.url && payload.url[0]
				const eventTarget = payload.target
				if (eventUrl && isString(eventUrl)) {
					updateLocalDomains()
					if (!some(localDomains, d => eventUrl.startsWith(d))) {
						return log('skipping event %o for non-local domain', eventUrl)
					}
					onEntryChange(eventUrl).catch(err => {
						log('Update processing error: %O', err)
					})
				} else if (eventTarget && isString(eventTarget)) {
					if (!some(localDomains, d => eventTarget.startsWith(d))) {
						return log('skipping webmention to %o for non-local domain', eventTarget)
					}
					onWebmention(payload).catch(err => {
						log('Webmention processing error: %O', err)
					})
				} else {
					return log('skipping unsupported pg pubsub event')
				}
			} catch (err) {
				return log('%O', err)
			}
		})
		ldb.query('LISTEN mf2_objects')
		ldb.query('LISTEN webmentions')
	},
	end (ldb) {
		ldb.end()
	}
})
notificationListener.try()

const liveHandler = async (ctx, next) => {
	const host = env.FAKE_HOST || ctx.request.host
	sse.subscribe(host, ctx.req, ctx.res)
	ctx.respond = false
	return next()
}

const robotsHandler = async (ctx, next) => {
	ctx.type = 'text/plain'
	ctx.body = `
User-agent: *
Disallow: /login
Disallow: /webmention
Disallow: /micropub
Disallow: /dist
Disallow: /live
`
	return next()
}

const logoutHandler = async (ctx, next) => {
	ctx.cookies.set('Bearer', '', { maxAge: -1 })
	ctx.redirect('back')
	return next()
}

const colorHandler = async ({ request, response }, next) => {
	response.type = 'text/css'
	if (request.query.rev && request.query.rev !== 'undefined') {
		response.header['cache-control'] += 'max-age=69420420, immutable'
	}
	response.body = await helpers.colorStyle(request.query)
	return next()
}

const searchHandler = async ({ request, response, auth, domainUriStr, tplctx }, next) => {
	const searchQuery = request.query.q || ''
	const { dobj, results, feeds, tags } = await db.row(SQL`SELECT
	set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
	mf2.objects_smart_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj,
	(SELECT jsonb_agg(row_to_json(subq)) FROM (
		SELECT
			type,
			properties,
			ts_headline(regexp_replace((objects.properties->'content'->0->'html')::text, E'<.*?>', '', 'g'), query, 'MaxFragments=2') AS snippet,
			ts_rank_cd(tsv, query, 32) AS rank
		FROM mf2.objects, plainto_tsquery(${searchQuery}) query
		WHERE query @@ tsv
		AND ('*' = ANY(acl) OR current_setting('mf2sql.current_user_url', true) = ANY(acl))
		AND properties->'url'->>0 LIKE ${domainUriStr + '%'}
		AND type @> '{h-entry}'
		AND deleted IS NOT True
		ORDER BY rank DESC
		LIMIT 64
	) subq) AS results,
	mf2.objects_fetch_feeds(${domainUriStr}) AS feeds,
	mf2.objects_fetch_categories(${domainUriStr}) AS tags
	`)
	tplctx.searchQuery = searchQuery
	tplctx.results = results
	tplctx.siteCard = get(dobj, 'properties.author[0]', {})
	tplctx.siteSettings = get(dobj, 'properties.site-settings[0]', {})
	tplctx.siteFeeds = feeds
	tplctx.siteTags = tags
	response.type = 'text/html'
	response.set('Referrer-Policy', 'no-referrer')
	response.body = await render('search.pug', tplctx)
	return next()
}

const handler = async ({ request, response, auth, domainUri, reqUri, reqUriFull, domainUriStr, reqUriStr, reqUriFullStr, tplctx, cashed }, next) => {
	if (cache && !auth && await cashed(2 * 60 * 1000)) return
	// TODO don't fetch twice when domain URI == request URI (i.e. home page)
	const perPage = 20
	const { dobj, obj, feeds, tags } = await db.row(SQL`SELECT
	set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
	mf2.objects_smart_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj,
	mf2.objects_smart_fetch(${reqUriStr}, ${domainUriStr}, ${perPage}, ${request.query.before || null}, ${request.query.after || null}, ${request.query}) AS obj,
	mf2.objects_fetch_feeds(${domainUriStr}) AS feeds,
	mf2.objects_fetch_categories(${domainUriStr}) AS tags
	`)

	// tplctx.perPage = perPage
	tplctx.siteCard = get(dobj, 'properties.author[0]', {})
	tplctx.siteSettings = get(dobj, 'properties.site-settings[0]', {})
	tplctx.siteFeeds = feeds
	tplctx.siteTags = tags
	response.status = 404
	response.type = 'text/html'
	let tpl = '404.pug'
	if (obj) {
		response.status = 200
		tplctx.obj = obj
		const authorizedPersonally = includes(obj.acl, (auth && auth.sub) || 'anonymous') || includes(obj.acl, (auth && (auth.sub + '/')) || 'anonymous')
		if (obj.deleted) {
			response.status = 410
			tpl = '410.pug'
		} else if (!(includes(obj.acl, '*') || authorizedPersonally)) {
			response.status = 401
			response.set('WWW-Authenticate', 'Bearer')
			tpl = '401.pug'
		} else if (includes(obj.type, 'h-entry') || includes(obj.type, 'h-review')) {
			tpl = 'entry.pug'
		} else if (includes(obj.type, 'h-feed') || includes(obj.type, 'h-x-dynamic-feed')) {
			if (!includes(obj.type, 'h-feed')) {
				obj.type.push('h-feed')
			}
			tpl = 'feed.pug'
		} else {
			log('Unknown entry type: %o', obj.type)
			tpl = 'unknown.pug'
		}
		response.set('Referrer-Policy', authorizedPersonally ? 'no-referrer' : 'no-referrer-when-downgrade')
	}
	response.body = await render(tpl, tplctx)
	return next()
}

const addCommonContext = async (ctx, next) => {
	const proto = env.FAKE_PROTO || ctx.request.protocol
	const host = env.FAKE_HOST || ctx.request.host
	ctx.domainUri = new URI(`${proto}://${host}/`).normalizePort().normalizeHostname()
	ctx.domainUriStr = ctx.domainUri.toString()
	ctx.reqUriFull = new URI(ctx.request.url).protocol(proto).hostname(host).normalizePort().normalizeHostname()
	ctx.reqUriFullStr = ctx.reqUriFull.toString()
	ctx.reqUri = ctx.reqUriFull.clone().search('')
	ctx.reqUriStr = ctx.reqUri.toString()
	ctx.tplctx = {
		// Libraries and our code
		moment,
		_,
		URI,
		qs,
		helpers,
		// Markup related stuff
		assets,
		// The Data
		domainUri: ctx.domainUri,
		reqUri: ctx.reqUri,
		reqUriFull: ctx.reqUriFull,
		requestUriStr: ctx.reqUriFullStr,
		auth: ctx.auth,
		// App settings
		indieAuthEndpoint,
		microPanelRoot,
		vapidKeys,
		// Pug settings
		basedir: './views',
		pretty: true,
		cache: cacheTemplates,
		plugins: [ pugTryCatch ],
	}
	ctx.link = new LinkHeader()
	ctx.link.set({ rel: 'webmention', uri: ctx.domainUri.clone().path('/webmention').toString() })
	ctx.link.set({ rel: 'micropub', uri: ctx.domainUri.clone().path('/micropub').toString() })
	ctx.link.set({ rel: 'token_endpoint', uri: ctx.domainUri.clone().path('/login').toString() })
	ctx.link.set({ rel: 'authorization_endpoint', uri: indieAuthEndpoint })
	ctx.link.set({ rel: 'hub', uri: websubHub })
	ctx.link.set({ rel: 'self', uri: ctx.reqUriStr })
	await next()
	ctx.response.set('Link', ctx.link.toString())
	// data: URI scripts are made by the HTML Imports polyfill, but that doesn't prevent buildled micro-panel from working
	const connectSrc = `connect-src 'self' ${mediaEndpoint.startsWith('/') ? '' : mediaEndpoint}`
	const scriptSrc = `script-src 'self' 'unsafe-eval' ${relaxCSP ? "data: 'unsafe-inline'" : "'sha256-8F+MddtNx9BXjGv2NKerT8QvmcOQy9sxZWMR6gaJgrU='"}`
	const formAction = `form-action 'self' ${indieAuthEndpoint.startsWith('/') ? '' : indieAuthEndpoint}`
	ctx.response.set('Content-Security-Policy', `default-src 'self'; ${scriptSrc}; style-src 'self' data: 'unsafe-inline'; img-src 'self' https: data:; media-src 'self' ${allowedCdns}; ${connectSrc}; ${formAction}${relaxCSP ? '' : "; frame-ancestors 'none'"}`)
	ctx.response.set('X-Frame-Options', 'DENY')
	ctx.response.set('X-XSS-Protection', '1; mode=block')
	ctx.response.set('X-Content-Type-Options', 'nosniff')
}

const koaCache = cache ? require('koa-cash')({
	hash (ctx) {
		// TODO filter out unused params
		return ctx.reqUriFullStr
	},
	async get (key, maxAge) {
		// log('cache get: %s', key)
		return cache.get(key)
	},
	async set (key, value, maxAge) {
		log('cache set: %s max age: %s', key, maxAge)
		return cache.set(key, value, maxAge)
	},
}) : null

const mount = require('koa-mount')
const compose = require('koa-compose')
const router = require('koa-better-router')()
if (!env.NO_SERVE_DIST) {
	const addImmutable = async (ctx, next) => {
		await next()
		ctx.response.header['cache-control'] += ', immutable'
	}
	router.addRoute('GET', '/dist/(.*)*',
		mount('/dist', compose([addImmutable, require('koa-better-serve')('./dist', '/', {
			maxage: 30 * 24 * 60 * 60 * 1000
		})]))
	)
}
router.addRoute('GET', '/live', liveHandler)
router.addRoute('GET', '/robots.txt', robotsHandler)
router.addRoute('POST', '/logout', logoutHandler)
router.addRoute('GET', '/color.css', colorHandler)
router.addRoute('GET', '/search', compose([addAuth, addCommonContext, koaCache, searchHandler].filter(isObject)))
router.options.notFound = compose([addAuth, addCommonContext, koaCache, handler].filter(isObject))
const app = new (require('koa'))()
if (isProxied) {
	app.proxy = true
}
app.use(router.middleware())
module.exports = app
