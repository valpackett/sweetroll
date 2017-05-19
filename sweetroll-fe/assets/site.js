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

document.getElementById('author-link').onclick = function () {
	var a = document.getElementById('author')
	a.setAttribute('tabindex', '-1')
	a.focus()
}

function loadJs (u) {
	return new Promise(function (resolve, reject) {
		const el = document.createElement('script')
		el.src = u
		el.onload = resolve
		el.onerror = reject
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

// We know these elements only use Custom Elements v1. Skip the loader and HTML imports to load faster.

function loadComponents () {
	if (document.querySelector('indie-action')) {
		loadJs('/dist/indieweb-components/indie-action.js')
	}
	if (document.querySelector('fragmention-target')) {
		loadJs('/dist/findAndReplaceDOMText/src/findAndReplaceDOMText.js')
			.then(function () { return loadJs('/dist/indieweb-components/fragmention-target.js') })
	}
	if (document.querySelector('simple-live')) {
		loadJs('/dist/indieweb-components/simple-live.js')
	}
}

var hasPanel = document.querySelector('micro-panel')

if (hasPanel && !('KeyframeEffect' in window && 'timeline' in document && 'play' in document.timeline)) {
	loadJs('/dist/web-animations-js/web-animations-next.min.js')
}

if ('customElements' in window && (!hasPanel || ('import' in document.createElement('link')))) {
	loadComponents()
} else {
	window.addEventListener('WebComponentsReady', loadComponents)
	if (!hasPanel) {
		// No need for Shady DOM unless using micro-panel (Polymer)
		Element.prototype.attachShadow = true
		Element.prototype.getRootNode = true
	}
	loadJs('/dist/webcomponentsjs/webcomponents-loader.js')
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
		if (data.event === 'refreshed-in-cache') {
			window.location.reload()
		}
	})
	if ((document.querySelector('meta[name=sw-offline]') || {}).content === 'true') {
		document.querySelector('.site-main').insertAdjacentHTML('afterbegin', '<div class="offline-popup">You are offline or on a slow connection. The page is still loading if you are not completely disconnected…</div>')
	}
}
