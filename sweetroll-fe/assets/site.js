/*! Sweetroll by https://unrelenting.technology */
/* global Element */

// the actual hiding happens in JS to prevent no-JS users from seeing nothing
const rs = document.querySelectorAll('.click-to-reveal')
const rts = document.querySelectorAll('.click-to-reveal-target')

for (var i = 0; i < rs.length; i++) {
	rs[i].hidden = false
	rs[i].addEventListener('click', function (e) {
		e.target.parentElement.parentElement.nextSibling.hidden = false
		e.target.parentElement.hidden = true
	})
}
for (i = 0; i < rts.length; i++) { rts[i].hidden = true }

const reloaders = document.querySelectorAll('.do-reload')
for (i = 0; i < reloaders.length; i++) {
	reloaders[i].addEventListener('click', function (e) {
		window.location.reload()
	})
}

function loadJs (u) {
	return new Promise(function (resolve, reject) {
		const el = document.createElement('script')
		el.src = u
		el.onload = resolve
		el.onerror = reject
		if (u.includes('micro-panel')) {
			el.setAttribute('data-manual', true) // prevent Prism from rehighlighting code on the page :D
		}
		document.body.appendChild(el)
	})
}

function loadCss (u) {
	return new Promise(function (resolve, reject) {
		const el = document.createElement('link')
		el.href = u
		el.rel = 'stylesheet'
		el.onload = resolve
		el.onerror = reject
		document.body.appendChild(el)
	})
}

const hasPanel = document.querySelector('micro-panel-editor')

function loadComponents () {
	if (!hasPanel && document.querySelector('indie-action')) {
		loadJs('/dist/indieweb-components/indie-action.js')
	}
	if (document.querySelector('fragmention-target')) {
		loadJs('/dist/findandreplacedomtext/src/findAndReplaceDOMText.js')
			.then(function () { return loadJs('/dist/indieweb-components/fragmention-target.js') })
	}
	if (document.querySelector('simple-live')) {
		loadJs('/dist/indieweb-components/simple-live.js')
	}
	if (hasPanel) {
		loadJs('/dist/micro-panel/dist/micro-panel-all.bundle.min.js')
	}
}

if ('customElements' in window && ('attachShadow' in HTMLElement.prototype || !hasPanel)) {
	loadComponents()
} else {
	window.addEventListener('WebComponentsReady', loadComponents)
	if (!hasPanel) {
		// No need for Shady DOM unless using micro-panel (lit-element)
		Element.prototype.attachShadow = true
		Element.prototype.getRootNode = true
	}
	loadJs('/dist/@webcomponents/webcomponentsjs/webcomponents-loader.js')
}

if (document.querySelector('.katex')) {
	loadCss('/dist/katex/dist/katex.css')
}

if ('serviceWorker' in navigator) {
	navigator.serviceWorker.register('/sw.js').then(reg => {
		if (hasPanel && 'PushManager' in window) {
			const lnk = document.createElement('a')
			lnk.href = '#'
			lnk.setAttribute('class', 'login-link')
			lnk.innerHTML = 'Request Push Notifications'
			lnk.addEventListener('click', e => {
				function urlBase64ToUint8Array (base64String) {
					const padding = '='.repeat((4 - base64String.length % 4) % 4)
					const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/')
					const rawData = window.atob(base64)
					const outputArray = new Uint8Array(rawData.length)
					for (let i = 0; i < rawData.length; ++i) {
						outputArray[i] = rawData.charCodeAt(i)
					}
					return outputArray
				}
				reg.pushManager.subscribe({
					userVisibleOnly: true,
					applicationServerKey: urlBase64ToUint8Array(document.querySelector('meta[name=vapid-pubkey]').content)
				}).then(sub => fetch('/micropub', {
					method: 'post',
					credentials: 'include',
					headers: { Accept: 'application/json', 'Content-Type': 'application/json' },
					body: JSON.stringify({
						action: 'update', url: window.location.origin + '/', add: { 'site-web-push-subscriptions': [sub] }
					})
				})).then(resp => alert('Subscription added!'))
					.catch(e => alert('Subscription error'))
			})
			const smeta = document.querySelector('.site-meta')
			smeta.parentElement.insertBefore(lnk, smeta)
		}
	})
	navigator.serviceWorker.addEventListener('message', e => {
		const data = JSON.parse(e.data)
		console.log(data)
		if (data.event === 'refreshed-in-cache') {
			window.location.reload()
		} else if (data.event === 'failed') {
			document.querySelector('.offline-popup').innerText = 'Failed to load the page: ' + data.err + '. This cached copy is all you have…'
		}
	})
	if ((document.querySelector('meta[name=sw-offline]') || {}).content === 'true') {
		const msg = navigator.onLine ? 'This page is still loading (either your connection is slow or this server is experiencing problems), it will be refreshed automatically. Maybe.' : 'You are offline, but this page has been cached locally.'
		document.querySelector('.site-main').insertAdjacentHTML('afterbegin', '<div class="offline-popup layer-sticky">' + msg + '</div>')
	}
}
