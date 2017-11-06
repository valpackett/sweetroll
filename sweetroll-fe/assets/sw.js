/*! Sweetroll by https://unrelenting.technology | thanks to https://serviceworke.rs */
/* global localforage */

importScripts('/dist/localforage/dist/localforage.js')

const PRECACHED_ASSETS = []
const SLOW_NET_TIMEOUT = 4000

/* Simple push handling (why isn't there built-in functionality for URL focus-or-open?!) */

self.addEventListener('push', e => {
	const data = e.data.json()
	e.waitUntil(
		self.registration.showNotification('Sweetroll', { body: data.body, data: data })
	)
})

self.addEventListener('notificationclick', e => {
	e.notification.close()
	const data = e.notification.data
	if (!data) {
		return
	}
	e.waitUntil(
		self.clients.matchAll({ type: 'window' }).then(clientList => {
			for (const client of clientList) {
				if (client.url === data.url) {
					return client.focus()
				}
			}
			return self.clients.openWindow(new URL(data.url))
		})
	)
})

/* Smart site caching */

self.addEventListener('install', e => {
	e.waitUntil((async () => {
		try {
			await caches.delete('assets')
			const cache = await caches.open('assets')
			await cache.addAll(PRECACHED_ASSETS)
		} catch (err) {
			console.error(err)
		}
	})())
	e.waitUntil((async () => {
		try {
			await caches.delete('fallback')
			const cache = await caches.open('fallback')
			await cache.addAll(['/offline.html'])
		} catch (err) {
			console.error(err)
		}
	})())
	e.waitUntil((async () => {
		try {
			const cache = caches.open('pages')
			await cache.addAll(['/'])
		} catch (err) {
			console.error(err)
		}
	})())
})

self.addEventListener('fetch', e => {
	if (e.request.method === 'GET') {
		if ((e.request.url.includes('/dist/') || e.request.url.includes('/color.css')) &&
			(e.request.url.includes('?') || e.request.url.endsWith('.woff2'))) {
			// Assets: return cached if exists, they're all cachebusted with query strings
			// Not all assets should be precached, every visitor does not need micro-panel :D
			e.respondWith(
				fromCache('assets', e.request).catch(() =>
					fromNetwork(e.request)
						.then(resp => {
							if (resp.status < 200 || resp.status >= 300) {
								return resp
							}
							const resp2 = resp.clone()
							caches.open('assets')
								.then(cache => cache.put(e.request, resp2))
							return resp
						})
				)
			)
		} else if (e.request.headers.get('accept').includes('text/html')) {
			// Pages: load from network, show cached (but keep loading) on timeout for slow connections
			e.respondWith((async () => {
				const jr = await localforage.getItem('just-refreshed')
				if ((jr || []).includes(e.request.url)) {
					console.log('SW | Returning %cjust-refreshed%c page %s', 'color: green', 'color: inherit', e.request.url)
					try {
						await localforage.setItem('just-refreshed', setRemove(jr, e.request.url))
						return fromCache('pages', e.request)
					} catch (err) {
						console.error(err)
						return fromNetwork(e.request)
					}
				}
				try {
					const resp = await fromNetwork(e.request, SLOW_NET_TIMEOUT)
					await addPageToCache(e.request)(resp)
					return resp
				} catch (err) {
					console.log('SW | %cTimeout%c for page %s / $O', 'color: red', 'color: inherit', e.request.url, err)
					// timeout failed so we refetch without timeout *asynchronously*
					fromNetwork(e.request).then(async resp => {
						await addPageToCache(e.request)(resp)
						if (isFailed(resp)) {
							console.log('SW | Fetched %cserver error%c page %s', 'color: orange', 'color: inherit', e.request.url)
							alertClients(e.request.url, await self.clients.matchAll(), { event: 'failed', err: 'error ' + resp.status })
						} else {
							const jr = localforage.getItem('just-refreshed')
							await localforage.setItem('just-refreshed', setAdd(jr, e.request.url))
							console.log('SW | Added %cjust-refreshed%c page %s', 'color: green', 'color: inherit', e.request.url)
							alertClients(e.request.url, await self.clients.matchAll(), { event: 'refreshed-in-cache' })
						}
					}, async err => {
						console.log('SW | Failed to fetch page %s', e.request.url)
						alertClients(e.request.url, await self.clients.matchAll(), { event: 'failed', err: err.message })
					})
					try {
						const resp = await fromCache('pages', e.request)
						console.log('SW | Returning %ccached%c page %s', 'color: orange', 'color: inherit', e.request.url)
						const meta = {
							status: resp.status,
							statusText: resp.statusText,
							headers: {}
						}
						const hkeys = resp.headers.keys()
						for (const k of hkeys) {
							meta.headers[k] = resp.headers.get(k)
						}
						const body = await resp.text()
						return new Response(body + '<meta name="sw-offline" content="true">', meta)
					} catch (err) {
						console.log('SW | Returning %cfallback%c page for %s (%O)', 'color: red', 'color: inherit', e.request.url, err)
						return fromCache('fallback', 'offline.html')
					}
				}
			})())
		}
	}
})

function alertClients (url, clientList, data) {
	for (const client of clientList) {
		if (client.url === url) {
			client.postMessage(JSON.stringify(data))
		}
	}
}

function isFailed (resp) {
	return resp.status < 200 || resp.status >= 300 || !(resp.headers.get('content-type') || '').includes('html')
}

function addPageToCache (req) {
	return async function (resp) {
		if (isFailed(resp)) {
			return resp
		}
		const pages = await localforage.getItem('cached-pages')
		await localforage.setItem('cached-pages', setAdd(pages, req.url))
		const cache = await caches.open('pages')
		await cache.put(req, resp.clone())
		return resp
	}
}

async function fromCache (cname, req) {
	const cache = await caches.open(cname)
	const matching = await cache.match(req)
	if (!matching) {
		throw new Error('no-match')
	}
	return matching
}

function fromNetwork (req, timeout) {
	return new Promise((resolve, reject) => {
		const timeoutId = timeout && setTimeout(reject, timeout)
		fetch(req).then(resp => {
			timeoutId && clearTimeout(timeoutId)
			resolve(resp)
		}, reject)
	})
}

function setAdd (a, v) {
	return Array.from(new Set(a || []).add(v))
}

function setRemove (a, v) {
	return Array.from(new Set(a || []).delete(v))
}
