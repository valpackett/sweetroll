this.onlyHttpUrl = function (x) {
	if (_.startsWith(x, '/') || _.startsWith(x, 'http://') || _.startsWith(x, 'https://') || _.startsWith(x, '//')) return x
	return 'javascript:void(0)'
}

this.isValidRef = function (url, x) {
	return _.some(x, function (v) {
		return _.some(url, function (u) {
			return _.startsWith(v, u) || _.startsWith(v.value, u)
		})
	})
}

this.separateComments = function (url, comments) {
	var result = { replies: [], likes: [], reposts: [], bookmarks: [], quotations: [] }
	_.forEach(comments, function (comment) {
		if (isValidRef(url, comment.properties['in-reply-to'])) {
			var text = _.trim(_.get(comment, 'properties.content[0].value', _.get(comment, 'properties.name[0]', 'xxxxxxxxxx')))
			if (text.length < 10) {
				var emoji = parseEmoji(text)
				if (emoji && emoji.length >= 1) {
					result[emoji] = result[emoji] || []
					result[emoji].push(comment)
					return
				}
			}
			result.replies.push(comment)
		} else if (isValidRef(url, comment.properties['like-of'])) {
			result.likes.push(comment)
		} else if (isValidRef(url, comment.properties['repost-of'])) {
			result.reposts.push(comment)
		} else if (isValidRef(url, comment.properties['bookmark-of'])) {
			result.bookmarks.push(comment)
		} else if (isValidRef(url, comment.properties['quotation-of'])) {
			result.quotations.push(comment)
		}
	})
	return result
}

SweetrollTemplates = {}
this.setTemplate = function (name, html) {
	SweetrollTemplates[name] = _.template(html, {
		'sourceURL': name, 'variable': 'scope',
		'imports': { 'templates': SweetrollTemplates }
	})
}
