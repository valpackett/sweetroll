this.Sweetroll = {
	_categoryDeciders: [
		function (props) {
			if (props['bookmark-of']) return { name: 'bookmarks', priority: 1 }
			if (props['like-of']) return { name: 'likes', priority: 1 }
			if (props['in-reply-to']) return { name: 'replies', priority: 1 }
			if (props.name) return { name: 'articles', priority: 1 }
			return { name: 'notes', priority: 1 }
		}
	]
}

Sweetroll._runCategoryDeciders = function (props) {
	return _(Sweetroll._categoryDeciders)
		.map(function (d) { try { return d(props) } catch (e) { return {} } })
		.filter(function (r) { return r && r.name && r.priority })
		.sortBy(function (r) { return -(r.priority || 1) })
		.head()
}

Sweetroll.addCategoryDecider = function (d) {
	if (_.isFunction(d))
		Sweetroll._categoryDeciders.push(d)
}
