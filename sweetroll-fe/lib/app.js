'use strict'
const helpers = require('./helpers')
const URI = require('urijs')
const moment = require('moment')
const Retry = require('promised-retry')
const _ = require('lodash')
const { concat, get, isObject } = _
const pify = require('pify')
const pug = require('pug')
const pg = require('pg')
const { default: PgAsync, SQL } = require('pg-async')

const env = process.env
const assets = require('dynamic-asset-rev')('dist')
const log = require('debug')('sweetroll-fe')
const sse = require('sse-broadcast')()
const cache = env.DO_CACHE ? require('lru-cache')({
	max: parseInt(env.CACHE_MAX_ITEMS || '128')
}) : null
const dburi = env.DATABASE_URI || 'postgres://localhost/sweetroll'
const db = new PgAsync(dburi)

const render = async (file, tplctx) =>
	pify(pug.renderFile)('views/' + file, tplctx)

const affectedUrls = async (url) => {
	const norm = new URI(url).normalizePort().normalizeHostname()
	const objUriStr = norm.clone().toString()
	const domainUriStr = norm.pathname('').search('').toString()
	const { obj, feeds } = await db.row(SQL`SELECT
		objects_smart_fetch(${objUriStr}, ${domainUriStr + '%'}, 1, null, null, null) AS obj,
		(SELECT jsonb_agg(obj) FROM (
			SELECT jsonb_build_object('type', type, 'properties', properties, 'children', children) AS obj
			FROM objects
			WHERE (properties->'url'->>0)::text LIKE ${domainUriStr + '%'}
			AND type @> '{h-x-dynamic-feed}'
			AND deleted IS NOT True
		) subq) AS feeds
	`)
	if (!obj) return [url]
	if (!feeds) return [url]
	return concat(helpers.matchingFeeds(feeds, obj).map(x => x.url), url)
}

const notificationListener = new Retry({
	name: 'notificationListener',
	log: require('debug')('retry:pg-listen'),
	try () {
		const ldb = new pg.Client(dburi)
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
			const eventUrl = JSON.parse(msg.payload).url[0]
			affectedUrls(eventUrl).then(urls => {
				log('got change notification for %s, affected URLs: %o', eventUrl, urls)
				if (cache) {
					for (const u of urls) {
						log('cache delete: %s', u)
						cache.del(u)
					}
				}
				sse.publish('changes', 'change', urls.join(','))
				// TODO partition sse publishes by domain
				// TODO WebSub publish
			}).catch(err => {
				console.error(err)
			})
		})
		ldb.query('LISTEN objects')
	},
	end (ldb) {
		ldb.end()
	}
})
notificationListener.try()

const liveHandler = async (ctx, next) => {
	sse.subscribe('changes', ctx.res)
	ctx.respond = false
	return next()
}

const handler = async ({ request, response, domainUri, reqUri, reqUriFull, domainUriStr, reqUriStr, reqUriFullStr, cashed }, next) => {
	// TODO full text search
	if (cache && await cashed(2 * 60 * 1000)) return
	// TODO don't fetch twice when domain URI == request URI (i.e. home page)
	const perPage = 20
	log(new Date(request.query.before))
	const { dobj, obj, feeds } = await db.row(SQL`SELECT
	objects_smart_fetch(${domainUriStr}, ${domainUriStr + '%'}, 1, null, null, null) AS dobj,
	objects_smart_fetch(${reqUriStr}, ${domainUriStr + '%'}, ${perPage + 5}, ${request.query.before || null}, ${request.query.after || null}, ${request.query}) AS obj,
	(SELECT jsonb_agg(obj) FROM (
		SELECT jsonb_build_object('type', type, 'properties', properties, 'children', children) AS obj
		FROM objects
		WHERE (properties->'url'->>0)::text LIKE ${domainUriStr + '%'}
		AND type @> '{h-x-dynamic-feed}'
		AND deleted IS NOT True
	) subq) AS feeds
	`)

	const tplctx = {
		// Libraries and our code
		moment,
		_,
		URI,
		helpers,
		// Markup related stuff
		assets,
		// The Data
		domainUri,
		reqUri,
		reqUriFull,
		requestUriStr: reqUriFullStr,
		perPage,
		siteCard: get(dobj, 'properties.author[0]', {}),
		siteSettings: get(dobj, 'properties.site-settings[0]', {}),
		siteFeeds: feeds,
		// App settings
		livereload: env.LIVE_RELOAD,
		// Pug settings
		basedir: './views',
		pretty: true,
		cache: env.CACHE_TEMPLATES,
	}
	response.status = 404
	response.type = 'text/html'
	let tpl = '404.pug'
	if (obj) {
		response.status = 200
		tplctx.obj = obj
		if (obj.deleted) {
			response.status = 410
			tpl = '410.pug'
		} else if (obj.type[0] === 'h-entry') {
			tpl = 'entry.pug'
		} else if (obj.type[0] === 'h-feed') {
			tpl = 'feed.pug'
		} else {
			console.error('Unknown entry type', obj.type)
			tpl = 'unknown.pug'
		}
	}
	response.body = await render(tpl, tplctx)
	return next()
}

const setUris = async (ctx, next) => {
	const proto = env.FAKE_PROTO || ctx.request.protocol
	const host = env.FAKE_HOST || ctx.request.host
	ctx.domainUri = new URI(`${proto}://${host}/`).normalizePort().normalizeHostname()
	ctx.domainUriStr = ctx.domainUri.toString()
	ctx.reqUriFull = new URI(ctx.request.url).protocol(proto).hostname(host).normalizePort().normalizeHostname()
	ctx.reqUriFullStr = ctx.reqUriFull.toString()
	ctx.reqUri = ctx.reqUriFull.clone().search('')
	ctx.reqUriStr = ctx.reqUri.toString()
	return next()
}

const koaCache = cache ? require('koa-cash')({
	hash (ctx) {
		return ctx.reqUriFullStr
	},
	async get (key, maxAge) {
		log('cache get: %s', key)
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
router.options.notFound = compose([setUris, koaCache, handler].filter(isObject))
const app = new (require('koa'))()
if (env.IS_PROXIED) {
	app.proxy = true
}
app.use(router.middleware())
module.exports = app
