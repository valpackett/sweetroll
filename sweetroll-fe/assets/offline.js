/*! Sweetroll by https://unrelenting.technology */
/* global localforage */

if (!navigator.onLine) {
	const title = document.getElementById('cached-title')
	title.innerText = 'You are offline'
	window.addEventListener('online', e => {
		location.reload(true)
	})
}

localforage.getItem('cached-pages').then(pages => {
	const list = document.getElementById('cached-list')
	for (const page of pages) {
		list.innerHTML += '<li><a href="' + page + '">' + page + '</a></li>'
	}
})
