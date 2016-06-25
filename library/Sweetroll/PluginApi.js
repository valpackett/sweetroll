this.Sweetroll = {
	conf: {},
	_categoryDeciders: [
		function (props) {
			if (!_.isEmpty(props['bookmark-of'])) return { name: 'bookmarks', priority: 1 }
			if (!_.isEmpty(props['like-of'])) return { name: 'likes', priority: 1 }
			if (!_.isEmpty(props['in-reply-to'])) return { name: 'replies', priority: 1 }
			if (!_.isEmpty(props.name)) return { name: 'articles', priority: 1 }
			return { name: 'notes', priority: 1 }
		}
	],
	_eventListeners: {}
}


/* --- Private API. called by Sweetroll --- */

Sweetroll._setConf = function (conf) {
	Sweetroll.conf = conf
}

Sweetroll._runCategoryDeciders = function (props) {
	return _(Sweetroll._categoryDeciders)
		.map(function (d) { try { return d(props) } catch (e) { return {} } })
		.filter(function (r) { return r && r.name && r.priority })
		.sortBy(function (r) { return -(r.priority || 1) })
		.head()
}

Sweetroll._fireEvent = function (name, data) {
	data.eventName = name
	_.forEach(Sweetroll._eventListeners[name] || [], function (listener) {
		try { listener(data) } catch (e) { print("Error in " + name + " event handler: " + e.stack) }
	})
}


/* --- Public API. called by plugins ---
 * Defined in Haskell:
 * * Sweetroll.fetch -- HTTP request */

Sweetroll.addCategoryDecider = function (d) {
	if (_.isFunction(d))
		Sweetroll._categoryDeciders.push(d)
}

Sweetroll.addEventListener = function (e, l) {
	if (_.isFunction(l)) {
		Sweetroll._eventListeners[e] = Sweetroll._eventListeners[e] || []
		Sweetroll._eventListeners[e].push(l)
	}
}
