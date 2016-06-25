// Pushover plugin for Sweetroll
// Note: only checks top level replies, i.e. does not notify for salmentions (nested replies)

var TOKEN = 'REDACTED -- USE YOUR OWN APP KEY'
var USER = 'REDACTED -- USE YOUR OWN USER KEY'

function presentContent (obj, fallback) {
	return _.truncate(_.get(obj, 'properties.name[0]',
			_.get(obj, 'properties.summary[0].value',
			_.get(obj, 'properties.summary[0].html',
			_.get(obj, 'properties.summary[0]',
			_.get(obj, 'properties.content[0].value',
			_.get(obj, 'properties.content[0].html',
			_.get(obj, 'properties.content[0]',
			fallback))))))), { length: 140 })
}

function propMentionsUrl (obj, prop, url) {
	return _.some(_.get(obj, 'properties.' + prop, []), function (v) {
		if (_.isString(v))
			return _.isEqual(v, url)
		return _.some(_.get(v, 'properties.url', []), _.partial(_.isEqual, url))
	})
}

function presentMention (postUrl, obj) {
	var result = '<b><a href="' + _.get(obj, 'properties.url[0]') + '">' + _.get(obj, 'properties.author[0].name[0]', _.get(obj, 'properties.author[0].value')) + '</a></b>'
	if (propMentionsUrl(obj, 'like-of', postUrl))
		result += ' <i>liked this</i> '
	else if (propMentionsUrl(obj, 'repost-of', postUrl))
		result += ' <i>reposted this</i> '
	else if (propMentionsUrl(obj, 'bookmark-of', postUrl))
		result += ' <i>bookmarked this</i> '
	else if (propMentionsUrl(obj, 'quotation-of', postUrl))
		result += ' <i>quoted this and said</i>: ' + presentContent(obj, '//no content//')
	else
		result += ': ' + presentContent(obj, '//no content//')
	return result
}

Sweetroll.addEventListener('update', function (event) {
	var oldObjMentions = _.get(event, 'oldObj.properties.comment', [])
	var newObjMentions = _.get(event, 'obj.properties.comment', [])
	var newMentions = _.filter(newObjMentions, function (cmt) {
		var found = _.find(oldObjMentions, _.matches(cmt))
		if (!found) return true
		// If it's an update, only notify if the content has changed -- don't bother with, like, the author's avatar change
		return !_.isEqual(_.get(found, 'properties.content'), _.get(cmt, 'properties.content'))
	})
	if (_.size(newMentions) < 1) {
		print("Pushover Plugin: no new/updated mentions for " + event.url)
	} else {
		print("Pushover Plugin: " + _.size(newMentions) + " new/updated mention(s) for " + event.url + "!")
		var resp = Sweetroll.fetch("https://api.pushover.net/1/messages.json", {
			method: 'POST',
			headers: { 'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8' },
			body: 'token=' + encodeURIComponent(TOKEN) +
				'&user=' + encodeURIComponent(USER) +
				'&title=' + encodeURIComponent('New mention' + (_.size(newMentions) == 1 ? '' : 's') +  ' on: ' + presentContent(event.obj, event.url)) +
				'&message=' + encodeURIComponent(_.join(_.map(newMentions, _.partial(presentMention, event.url)), '\n\n')) +
				'&url=' + encodeURIComponent(event.url) +
				'&html=1'
		})
		print("Pushover Plugin: " + _.size(newMentions) + " new/updated mention(s) for " + event.url + " -- result: " + resp.status + ": " + resp.body)
	}
})
