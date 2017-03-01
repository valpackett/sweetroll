'use strict'
const { join } = require('path')
const Funnel = require('broccoli-funnel')
const MergeTrees = require('broccoli-merge-trees')
const SVGStore = require('broccoli-svgstore')
const Zopfli = require('broccoli-zopfli')
const Brotli = require('broccoli-brotli')
const { SourceMapExtractor } = require('broccoli-source-map')
const Pug = require('broccoli-pug-render')
const PostCSS = require('broccoli-postcss')
const Concat = require('broccoli-concat')

const bowerdeps = new Funnel('bower_components', {
	include: [
		'webcomponentsjs/*.js',
		'findAndReplaceDOMText/src/*.js',
		'svgxuse/*.min.js',
		'lazyload-image/lazyload-image.html',
		'indieweb-components/*.{html,js}'
	]
})

const scripts = new Funnel('assets', { include: [ '*.js' ] })



let styles = new Concat(new MergeTrees([
	'assets',
	new Funnel('bower_components', { include: ['sanitize-css/*.css', 'normalize-opentype.css/*.css'] })
]), {
	outputFile: 'style.css',
	inputFiles: ['sanitize-css/sanitize.css', 'normalize-opentype.css/normalize-opentype.css', 'style.css']
})

styles = new SourceMapExtractor(new PostCSS(styles, {
	plugins: [
		{ module: require('postcss-nesting') },
		{ module: require('postcss-flexbugs-fixes') },
		{ module: require('postcss-responsive-type') },
		{ module: require('postcss-css-variables') },
		{ module: require('postcss-color-function') },
		{ module: require('autoprefixer') },
		{ module: require('cssnano') },
	],
	from: 'assets/style.css',
	map: { inline: true, annotation: true }
}))

const icons = new SVGStore(
	new Funnel('bower_components/Font-Awesome-SVG-PNG/black/svg/', {
		include: ['arrow-up', 'arrow-down', 'reply', 'retweet', 'star', 'info-circle', 'bookmark', 'quote-left'].map((x) => x + '.svg')
	}),
	{ outputFile: 'icons.svg' }
)

const errPages = new Pug(new Funnel('views', {
	include: ['403.pug', '404.pug', '500.pug', '502.pug', '503.pug', '504.pug']
}), {
	basedir: join(__dirname, '/views'),
	pretty: true,
	_: require('lodash'),
	assets: require('dynamic-asset-rev')('dist'), // yeah ?undefined cache busting but that's fine
	siteSettings: { }
})

const all = new MergeTrees([
	bowerdeps, styles, scripts, icons, errPages
])

const compressExts = ['js', 'css', 'svg', 'html']

module.exports = new MergeTrees([
	all,
	new Zopfli(all, { extensions: compressExts }),
	new Brotli(all, { extensions: compressExts })
])
