/* global Element */
/*! Sweetroll by https://unrelenting.technology */

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

if (window.customElements && (!hasPanel || ('import' in document.createElement('link')))) {
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
