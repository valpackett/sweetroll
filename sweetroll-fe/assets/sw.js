/*! Sweetroll by https://unrelenting.technology | thanks to https://serviceworke.rs */

importScripts('/dist/localforage/dist/localforage.js')

const PRECACHED_ASSETS = []
const SLOW_NET_TIMEOUT = 3000

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
	return caches.delete('assets')
		.then(() => caches.open('assets'))
		.then(cache => cache.addAll(PRECACHED_ASSETS))
		.then(() => caches.delete('fallback'))
		.then(() => caches.open('fallback'))
		.then(cache => cache.addAll(['/offline.html']))
		.then(() => caches.open('pages'))
		.then(cache => cache.addAll(['/']))
})

self.addEventListener('fetch', e => {
	if (e.request.method === 'GET') {
		if (e.request.url.includes('/dist/') && e.request.url.includes('?')) {
			// Assets: just return precached if exists, they're all cachebusted with query strings
			e.respondWith(fromCache('assets', e.request).catch(() => fromNetwork(e.request)))
		} else if (e.request.headers.get('accept').includes('text/html')) {
			// Pages: load from network, show cached (but keep loading) on timeout for slow connections
			e.respondWith(
				localforage.getItem('just-refreshed').then(jr => {
					if ((jr || []).includes(e.request.url)) {
						console.log('SW | Returning %cjust-refreshed%c page %s', 'color: green', 'color: inherit', e.request.url)
						return localforage.setItem('just-refreshed', setRemove(jr, e.request.url))
							.then(() => fromCache('pages', e.request))
							.catch(() => fromNetwork(e.request))
					}
					return fromNetwork(e.request, SLOW_NET_TIMEOUT)
						.then(addPageToCache(e.request))
						.catch(err => {
							console.log('SW | %cTimeout%c for page %s', 'color: red', 'color: inherit', e.request.url)
							fromNetwork(e.request).then(addPageToCache(e.request))
								.then(() => localforage.getItem('just-refreshed'))
								.then(jr => localforage.setItem('just-refreshed', setAdd(jr, e.request.url)))
								.then(() => self.clients.matchAll())
								.then(clientList => {
									console.log('SW | Added %cjust-refreshed%c page %s', 'color: green', 'color: inherit', e.request.url)
									for (const client of clientList) {
										if (client.url === e.request.url) {
											client.postMessage(JSON.stringify({
												event: 'refreshed-in-cache'
											}))
										}
									}
								})
							return fromCache('pages', e.request).then(resp => {
								console.log('SW | Returning %ccached%c page %s', 'color: orange', 'color: inherit', e.request.url)
								const meta = {
									status: resp.status,
									statusText: resp.statusText,
									headers: {}
								}
								const hkeys = resp.headers.keys()
								for (const k of resp.headers.keys()) {
									meta.headers[k] = resp.headers.get(k)
								}
								return resp.text().then(body => new Response(body + '<meta name="sw-offline" content="true">', meta))
							}).catch(err => {
								console.log('SW | Returning %cfallback%c page for %s (%O)', 'color: red', 'color: inherit', e.request.url, err)
								return fromCache('fallback', 'offline.html')
							})
					})
				})
			)
		}
	}
})

function addPageToCache (req) {
	return function (resp) {
		if (resp.status < 200 || resp.status >= 300) {
			return Promise.resolve(resp)
		}
		return localforage.getItem('cached-pages')
			.then(pages => localforage.setItem('cached-pages', setAdd(pages, req.url)))
			.then(() => caches.open('pages'))
			.then(cache => cache.put(req, resp.clone()))
			.then(() => resp)
	}
}

function fromCache (cname, req) {
	return caches.open(cname).then(cache => {
		return cache.match(req).then(matching => {
			return matching || Promise.reject('no-match')
		})
	})
}

function fromNetwork (req, timeout) {
	return new Promise((fulfill, reject) => {
		const timeoutId = timeout && setTimeout(reject, timeout)
		fetch(req).then(resp => {
			timeoutId && clearTimeout(timeoutId)
			fulfill(resp)
		}, reject)
	})
}

function setAdd (a, v) {
	return Array.from(new Set(a || []).add(v))
}

function setRemove (a, v) {
	return Array.from(new Set(a || []).delete(v))
}
