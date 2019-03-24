'use strict'

import * as helpers from './helpers'
import URI from 'urijs'
import LinkHeader from 'http-link-header'
import moment from 'moment'
import fetch from 'node-fetch'
import webpush from 'web-push'
import qs from 'qs'
import Retry from 'promised-retry'
import _ from 'lodash'
const { head, merge, concat, get, isObject, groupBy, sortBy, includes, isString, debounce, some, flatMap } = _
import revHash from 'rev-hash'
import pify from 'pify'
import pug from 'pug'
import pugTryCatch from 'pug-plugin-try-catch'
import pg from 'pg'
import pgasync from 'pg-async'
const PgAsync = pgasync.default
const { SQL } = PgAsync
import debug from 'debug'
import ssecast from 'sse-broadcast'
import assrev from 'dynamic-asset-rev'
import lru from 'lru-cache'
import pgstring from 'pg-connection-string'
import jsonwebtoken from 'jsonwebtoken'
import common from '../../sweetroll-node-common'

const env = process.env
const userAgent = 'Sweetroll'
const assets = assrev('dist')
const log = debug('sweetroll-fe')
const sse = ssecast({ compression: true })
const isProxied = env.IS_PROXIED // Whether to trust X-Forwarded-Proto
const cacheTemplates = env.CACHE_TEMPLATES // Do not recompile templates on every request. Enable in prod for perf boost
const cache = env.DO_CACHE ? lru({
	max: parseInt(env.CACHE_MAX_ITEMS || '128')
}) : null
const granaryUrl = env.GRANARY_URL || 'https://granary-demo.appspot.com/url'
const websubHub = env.WEBSUB_HUB || 'https://switchboard.p3k.io'
const websubHubMode = env.WEBSUB_HUB_MODE || 'multi' // Whether to do one request for all URLs affected by a change or one request per URL
const indieAuthEndpoint = env.INDIEAUTH_ENDPOINT || 'https://indieauth.com/auth'
const mediaEndpoint = env.MEDIA_ENDPOINT || '/micropub/media'
const mediaAuthToken = env.MEDIA_AUTH_TOKEN // Simple token auth for having the endpoint on a different domain & keeping cookies httpOnly
const webmentionOutbox = env.WEBMENTION_OUTBOX // Allow using an external sender like Telegraph, default to sending on our own
const webmentionOutboxConf = JSON.parse(env.WEBMENTION_OUTBOX_CONF || '{}') // Something like {token: '...'} for Telegraph
const allowedCdns = env.ALLOWED_CDNS || '' // List of allowed CDN domains for Content-Security-Policy
const vapidKeys = { publicKey: env.VAPID_PUBLIC_KEY, privateKey: env.VAPID_PRIVATE_KEY, contact: env.VAPID_CONTACT } // Web Push keypair
if (vapidKeys.publicKey) {
	webpush.setVapidDetails(vapidKeys.contact, vapidKeys.publicKey, vapidKeys.privateKey)
}
const dbsettings = pgstring.parse(env.DATABASE_URL || env.DATABASE_URI || 'postgres://localhost/sweetroll')
dbsettings.application_name = 'sweetroll-fe'
dbsettings.max = parseInt(env.PG_POOL_MAX || '4')
dbsettings.ssl = env.PG_SSL
const db = new PgAsync(dbsettings)

const addAuth = common.authentication(log, jsonwebtoken.verify)

const pugRender = pify(pug.renderFile)
const render = async (file, tplctx) =>
	pugRender('views/' + file, tplctx)

const granaries = [
	{ base: `${granaryUrl}?input=html&output=as2&hub=${encodeURIComponent(websubHub)}`, type: 'application/activity+json' },
	{ base: `${granaryUrl}?input=html&output=atom&hub=${encodeURIComponent(websubHub)}`, type: 'application/atom+xml' },
	{ base: `${granaryUrl}?input=html&output=jsonfeed&hub=${encodeURIComponent(websubHub)}`, type: 'application/json' },
]

const affectedUrls = async (url) => {
	const norm = new URI(url).normalizePort().normalizeHostname()
	const objUriStr = norm.clone().toString()
	const domainUriStr = norm.pathname('').search('').toString()
	const { obj, feeds } = await db.row(SQL`SELECT
		mf2.objects_fetch(${objUriStr}, ${domainUriStr}, 1, null, null, null) AS obj,
		mf2.objects_fetch_feeds(${domainUriStr}) AS feeds
	`)
	if (!obj || !feeds) return { affUrls: [url], domainUriStr }
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
			'User-Agent': userAgent,
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
	const { dobj } = await db.row(SQL`SELECT mf2.objects_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj`)

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
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8',
					'User-Agent': userAgent,
				},
				body: qs.stringify({ 'hub.mode': 'publish', 'hub.topic': urls }, { arrayFormat: 'brackets' }),
			})
			log('WebSub hub response: %s %s %O', resp.status, resp.statusText, await resp.text())
		} catch (err) {
			log('Could not ping the WebSub hub: %O', err)
		}
	}
	if (websubHubMode === 'multi') {
		pushWebSub(affUrls)
		pushWebSub(flatMap(affUrls, url => flatMap(granaries, ({base}) => `${base}&url=${encodeURIComponent(url)}`)))
	} else {
		for (const url of affUrls) {
			pushWebSub(url)
			for (const { base } of granaries) {
				pushWebSub(`${base}&url=${encodeURIComponent(url)}`)
			}
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
				headers: {
					'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8',
					'User-Agent': userAgent,
				},
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
	log: debug('retry:pg-listen'),
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

const indieConfigHandler = async ({ response, tplctx, cashed }, next) => {
	if (cache && await cashed(100 * 2 * 60 * 1000)) {
		return
	}
	response.body = await render('indieconfig.pug', tplctx)
	response.set('Content-Security-Policy', `default-src 'self'; script-src 'unsafe-inline'; frame-ancestors http: https:`)
	return next()
}

const customCssHandler = async ({ request, response, auth, domainUriStr, cashed }, next) => {
	if (cache && !auth && await cashed(2 * 60 * 1000)) return
	response.type = 'text/css'
	if (request.query.rev && request.query.rev !== 'undefined' && request.query.rev !== 'null') {
		response.set('Cache-Control', 'max-age=69420420, immutable')
	}
	const { dobj } = await db.row(SQL`SELECT
	set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
	mf2.objects_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj`)
	response.body = (dobj.properties['site-css'] || []).join('\n')
	return next()
}

const searchHandler = async ({ request, response, auth, domainUriStr, tplctx }, next) => {
	const searchQuery = request.query.q || ''
	const { dobj, results, feeds, tags } = await db.row(SQL`SELECT
	set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
	mf2.objects_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS dobj,
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
	tplctx.siteCss = get(dobj, 'properties.site-css', [])
	tplctx.siteFeeds = feeds
	tplctx.siteTags = tags
	response.type = 'text/html'
	response.set('Referrer-Policy', 'no-referrer')
	response.body = await render('search.pug', tplctx)
	return next()
}

const handler = async ({ request, response, auth, domainUri, reqUri, reqUriFull, domainUriStr, reqUriStr, reqUriFullStr, tplctx, cashed }, next) => {
	/// SW TEST: await new Promise(res => setTimeout(res, 6000)); response.status = 500; return 0
	if (cache && !auth && await cashed(2 * 60 * 1000)) {
		return
	}
	const perPage = reqUriStr.endsWith('/kb') ? 999999 : 20 // XXX: ugly hardcode but avoids a roundtrip :D
	const { _dobj, obj, feeds, tags } = await db.row(
		reqUriStr == domainUriStr // don't fetch twice
		? SQL`SELECT
			set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
			NULL as _dobj,
			mf2.objects_fetch(${reqUriStr}, ${domainUriStr}, ${perPage}, ${request.query.before || null}, ${request.query.after || null}, ${request.query}) AS obj,
			mf2.objects_fetch_feeds(${domainUriStr}) AS feeds,
			mf2.objects_fetch_categories(${domainUriStr}) AS tags`
		: SQL`SELECT
			set_config('mf2sql.current_user_url', ${(auth && auth.sub) || 'anonymous'}, true),
			mf2.objects_fetch(${domainUriStr}, ${domainUriStr}, 1, null, null, null) AS _dobj,
			mf2.objects_fetch(${reqUriStr}, ${domainUriStr}, ${perPage}, ${request.query.before || null}, ${request.query.after || null}, ${request.query}) AS obj,
			mf2.objects_fetch_feeds(${domainUriStr}) AS feeds,
			mf2.objects_fetch_categories(${domainUriStr}) AS tags`
	)
	const dobj = reqUriStr == domainUriStr ? obj : _dobj

	// tplctx.perPage = perPage
	tplctx.siteCard = get(dobj, 'properties.author[0]', {})
	tplctx.siteSettings = get(dobj, 'properties.site-settings[0]', {})
	tplctx.siteCss = get(dobj, 'properties.site-css', [])
	tplctx.siteFeeds = feeds
	tplctx.siteTags = tags
	response.status = 404
	response.type = 'text/html'
	let tpl = '404.pug'
	if (obj) {
		response.status = 200
		tplctx.obj = obj
		const authorizedPersonally = includes(obj.acl, (auth && auth.sub && auth.sub.replace(/\/$/, "")) || 'anonymous') ||
			includes(obj.acl, (auth && (auth.sub + '/')) || 'anonymous')
		if (obj.deleted) {
			response.status = 410
			tpl = '410.pug'
		} else if (!(includes(obj.acl, '*') || authorizedPersonally)) {
			response.status = 401
			response.set('WWW-Authenticate', 'Bearer')
			tpl = '401.pug'
		} else if (includes(obj.type, 'h-entry') || includes(obj.type, 'h-review')) {
			tpl = 'entry.pug'
		} else if (includes(obj.type, 'h-feed') || includes(obj.type, 'h-x-dynamic-feed') || includes(obj.type, 'h-x-reader-channel')) {
			if (!includes(obj.type, 'h-feed')) {
				obj.type.push('h-feed')
			}
			tplctx.feedSettings = get(obj, 'properties.feed-settings[0]') || {}
			if (tplctx.feedSettings.sort) {
				obj.children = sortBy(obj.children || [], tplctx.feedSettings.sort)
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
	const basedir = assets.basedir = './views'
	ctx.tplctx = {
		// Libraries and our code
		moment,
		_,
		URI,
		qs,
		helpers,
		revHash,
		// Markup related stuff
		assets,
		// The Data
		domainUri: ctx.domainUri,
		reqUri: ctx.reqUri,
		reqUriFull: ctx.reqUriFull,
		requestUriStr: ctx.reqUriFullStr,
		auth: ctx.auth,
		authedAsAdmin: ctx.auth && new URI(ctx.auth.sub).equals(ctx.domainUri),
		// App settings
		mediaEndpoint,
		mediaAuthToken,
		indieAuthEndpoint,
		vapidKeys,
		granaries,
		// Pug settings
		basedir,
		pretty: true,
		cache: cacheTemplates,
		plugins: [ pugTryCatch ],
	}
	ctx.link = new LinkHeader()
	ctx.link.set({ rel: 'webmention', uri: ctx.domainUri.clone().path('/webmention').toString() })
	ctx.link.set({ rel: 'micropub', uri: ctx.domainUri.clone().path('/micropub').toString() })
	ctx.link.set({ rel: 'microsub', uri: ctx.domainUri.clone().path('/microsub').toString() })
	ctx.link.set({ rel: 'token_endpoint', uri: ctx.domainUri.clone().path('/login').toString() })
	ctx.link.set({ rel: 'authorization_endpoint', uri: indieAuthEndpoint })
	ctx.link.set({ rel: 'hub', uri: websubHub })
	ctx.link.set({ rel: 'self', uri: ctx.reqUriStr })
	await next()
	ctx.response.set('Link', ctx.link.toString())
	const scriptSrc = `script-src 'self' 'unsafe-eval'`
	const styleSrc = `style-src 'self' data: 'unsafe-inline'`
	const imgSrc = `img-src 'self' https: data:`
	const mediaSrc = `media-src 'self' ${allowedCdns}`
	const connectSrc = `connect-src 'self' ${mediaEndpoint.startsWith('/') ? '' : mediaEndpoint} https://*.openstreetmap.org`
	const frameSrc = `frame-src web+action: https:`
	const formAction = `form-action 'self' ${indieAuthEndpoint.startsWith('/') ? '' : indieAuthEndpoint}`
	if (!ctx.response.get('Content-Security-Policy')) {
		ctx.response.set('Content-Security-Policy', `default-src 'self'; ${scriptSrc}; ${styleSrc}; ${imgSrc}; ${mediaSrc}; ${connectSrc}; ${frameSrc}; ${formAction}; frame-ancestors 'none'`)
		ctx.response.set('X-Frame-Options', 'DENY')
	}
	ctx.response.set('X-XSS-Protection', '1; mode=block')
	ctx.response.set('X-Content-Type-Options', 'nosniff')
	ctx.response.set('Feature-Policy', `unsized-media 'none'; sync-xhr 'none'; document-write 'none'`)
}

import koaCash from 'koa-cash'
const koaCache = cache ? koaCash({
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

import Koa from 'koa'
import mount from 'koa-mount'
import compose from 'koa-compose'
import betterRouter from 'koa-better-router'
import betterServe from 'koa-better-serve'
const router = betterRouter()
if (!env.NO_SERVE_DIST) {
	const addImmutable = async (ctx, next) => {
		await next()
		ctx.response.header['cache-control'] += ', immutable'
	}
	router.addRoute('GET', '/dist/(.*)*',
		mount('/dist', compose([addImmutable, betterServe('./dist', '/', {
			maxage: 30 * 24 * 60 * 60 * 1000
		})]))
	)
}
router.addRoute('GET', '/live', liveHandler)
router.addRoute('GET', '/robots.txt', robotsHandler)
router.addRoute('POST', '/logout', logoutHandler)
router.addRoute('GET', '/_indie-config', compose([addAuth, addCommonContext, koaCache, indieConfigHandler].filter(isObject)))
router.addRoute('GET', '/custom.css', compose([addAuth, addCommonContext, koaCache, customCssHandler].filter(isObject)))
router.addRoute('GET', '/search', compose([addAuth, addCommonContext, koaCache, searchHandler].filter(isObject)))
router.options.notFound = compose([addAuth, addCommonContext, koaCache, handler].filter(isObject))
const app = new Koa()
if (isProxied) {
	app.proxy = true
}
app.use(router.middleware())
export default app
