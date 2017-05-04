'use strict'

// There's no strict rule about "what is a helper" but generally,
// helpers are about data manipulation.

const { head, some, trim, get, eq, includes, concat, flatMap, isString, isArray, isObject, difference } = require('lodash')
const { count: countEmoji } = require('emoji-king')
const URI = require('urijs')
const cheerio = require('cheerio')
const gravatarUrl = require('gravatar-url')
const log = require('debug')('sweetroll-fe:helpers')

module.exports = {
	insertParams (name, params) {
		// TODO: make this more efficient
		let result = name
		for (const k of Object.keys(params)) {
			result = result.replace(`{${k}}`, params[k])
		}
		return result
	},

	matchingFeeds (feeds, obj) {
		return flatMap(feeds, feed => flatMap(feed.properties.filter, filter => {
			let matches = true
			const params = {}
			const walk = (x, y) => {
				for (const k of Object.keys(x)) {
					if (x[k].length === 1 && isString(x[k][0]) && x[k][0].startsWith('{') && x[k][0].endsWith('}') && isArray(y[k])) {
						const p = trim(x[k][0], '{}')
						params[p] = concat(params[p] || [], y[k])
					} else if (isArray(x[k]) && isArray(y[k])) {
						matches = matches && difference(x[k], y[k]).length === 0
					} else if (isObject(x[k]) && isObject(y[k])) {
						walk(x[k], y[k])
					} else {
						matches = matches && eq(x[k], y[k])
					}
					if (!matches) return
				}
			}
			walk(filter, obj.properties)
			if (matches) {
				const base = new URI(feed.properties.url[0])
				if (Object.keys(params).length === 0) {
					return [{
						name: get(feed, 'properties.name[0]', ''),
						url: base.toString(),
						feedObj: feed,
						params: {}
					}]
				}
				return flatMap(Object.keys(params), k =>
					params[k].map(v => {
						const params = {[k]: v}
						return {
							name: this.insertParams(get(feed, 'properties.name[0]', ''), params),
							url: base.clone().search(params).toString(),
							feedObj: feed,
							params
						}
					}))
			}
		})).filter(isObject)
	},

	onlyHttpUrl (x) {
		if (!x) return 'javascript:void(0)'
		if (x.startsWith('/') || x.startsWith('http://') || x.startsWith('https://') || x.startsWith('//')) return x
		return 'javascript:void(0)'
	},

	isValidRef (url, x) {
		return some(x, v => some(url, u => v && ((v.startsWith && v.startsWith(u)) || (v.value && v.value.startsWith && v.value.startsWith(u)))))
	},

	separateComments (url, comments) {
		const result = { replies: [], likes: [], reposts: [], bookmarks: [], quotations: [] }
		for (const comment of comments) {
			const properties = comment.properties || {}
			if (this.isValidRef(url, properties['in-reply-to'])) {
				const text = trim(get(properties, 'content[0].value', get(properties, 'name[0]', 'xxxxxxxxxx')))
				if (text.length < 16) {
					const emoji = countEmoji(text)
					if (emoji && Object.keys(emoji).length >= 1) {
						for (const e of Object.keys(emoji)) {
							result[e] = result[e] || []
							result[e].push(comment)
						}
						return
					}
				}
				result.replies.push(comment)
			} else if (this.isValidRef(url, properties['like-of'])) {
				result.likes.push(comment)
			} else if (this.isValidRef(url, properties['repost-of'])) {
				result.reposts.push(comment)
			} else if (this.isValidRef(url, properties['bookmark-of'])) {
				result.bookmarks.push(comment)
			} else if (this.isValidRef(url, properties['quotation-of'])) {
				result.quotations.push(comment)
			} else {
				log('Unknown mention type for object %O', comment)
			}
		}
		return result
	},

	syndicationName (x) {
		if (includes(x, 'twitter.com')) return 'Twitter'
		if (includes(x, 'tumblr.com')) return 'Tumblr'
		if (includes(x, 'facebook.com')) return 'Facebook'
		if (includes(x, 'instagram.com')) return 'Instagram'
		const matches = x.match(/^https?:\/\/([^/?#]+)/)
		return (matches && matches[1]) || x
	},

	showAvatar (author) {
		return get(author, 'properties.photo[0]') ||
			gravatarUrl(
				get(author, 'properties.email[0]', 'test@example.com').replace('mailto:', ''),
				{ size: 256, default: 'identicon' }
			)
	},

	entryName (obj) {
		return get(obj, 'properties.name[0]') || get(obj, 'properties.published[0]', 'Untitled page')
	},

	getContent (properties, long, onlySummary) {
		let content = head(onlySummary ? properties.summary : (
			long
				? concat(properties.content || [], properties.summary || [], properties.name || [])
				: concat(properties.summary || [], properties.content || [], properties.name || [])
			)
		) || ''
		content = isObject(content) ? (content.html || content.value) : content
		content = isString(content) ? content : null
		return content
	},

	processContent (content) {
		if (!content) return content
		// NOTE: do not use => functions for cheerio!
		const $ = cheerio.load(content)
		// detwitterize emoji
		$('img.Emoji').each(function (i) {
			const img = $(this)
			img.replaceWith(`<span class="emoji">${img.attr('alt')}</span>`)
		})
		$('img').each(function (i) {
			const img = $(this)
			img.attr('is', 'lazyload-image')
			// TODO: HTTPS proxy
		})
		return $.html()
	},

	findMentionedLinks (obj) {
		const fromCtxs = concat(
			get(obj, 'properties.in-reply-to', []),
			get(obj, 'properties.like-of', []),
			get(obj, 'properties.repost-of', []),
			get(obj, 'properties.quotation-of', [])
		).map(ctx => isObject(ctx) ? get(ctx, 'properties.url.0') : ctx)
		let fromContent = []
		try {
			const $ = cheerio.load(this.getContent(obj.properties, true, false))
			fromContent = $('a, link').toArray().map(el => el.attribs['href'])
		} catch (err) {
			log('HTML content parse error: %O', err)
		}
		return concat(fromCtxs, fromContent).filter(isString)
	},

	getHtmlLinksByRel (content) {
		const $ = cheerio.load(content)
		return $('link[rel], a[rel]').toArray()
			.filter(el => includes(el.attribs['rel'], 'webmention'))
			.map(el => el.attribs['href'])
	}

}
