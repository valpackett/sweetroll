'use strict'

const { join } = require('path')
const { readFileSync } = require('fs')
const Funnel = require('broccoli-funnel')
const MergeTrees = require('broccoli-merge-trees')
const AssetRev = require('broccoli-static-asset-rev')
const ConfigReplace = require('broccoli-config-replace')
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
		'web-animations-js/web-animations-next.min.js',
		'findAndReplaceDOMText/src/*.js',
		'svgxuse/*.min.js',
		'localforage/dist/localforage.js',
		'lazyload-image/lazyload-image.html',
		'indieweb-components/*.{html,js}'
	],
	exclude: [ 'webcomponentsjs/gulpfile.js' ]
})

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
		{ module: require('postcss-responsive-type') },
		{ module: require('postcss-flexbugs-fixes') },
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
		include: ['arrow-up', 'arrow-down', 'reply', 'retweet', 'link',
			'star', 'star-half-o', 'star-o', 'info-circle', 'bookmark', 'quote-left', 'lock'].map((x) => x + '.svg')
	}),
	{ outputFile: 'icons.svg' }
)

let micropanel = new Funnel('../micro-panel/dist', {
	destDir: 'micro-panel',
	exclude: [ 'bower_components' ]
})

let rev = new AssetRev([ bowerdeps, micropanel, styles, icons ])

const scripts = new ConfigReplace(
	new Funnel('assets', { include: [ 'site.js', 'offline.js' ] }),
	rev, {
		files: [ 'site.js', 'offline.js' ],
		configPath: 'assets.json',
		patterns: [{
			match: /['"]\/dist\/([^'"]+)['"]/g,
			replacement: (assets, match, url) => `'/dist/${url}?${assets[url]}'`
		}]
	}
)

rev = new AssetRev([ bowerdeps, micropanel, styles, icons, scripts ]) // TODO: append instead of recompute

const sw = new ConfigReplace(
	new Funnel('assets', { include: [ 'sw.js' ] }),
	rev, {
		files: [ 'sw.js' ],
		configPath: 'assets.json',
		patterns: [{
			match: /['"]\/dist\/([^'"]+)['"]/g,
			replacement: (assets, match, url) => `'/dist/${url}?${assets[url]}'`
		}, {
			match: /PRECACHED_ASSETS\s*=\s*\[\]/g,
			replacement: (assets, _) => `PRECACHED_ASSETS = ${JSON.stringify(Object.keys(assets).map(url => `/dist/${url}?${assets[url]}`))}`
		}]
	}
)

const errPages = new Pug(new MergeTrees([
	new Funnel('views', {
		include: ['401.pug', '403.pug', '404.pug', '429.pug', '500.pug', '502.pug', '503.pug', '504.pug', 'offline.pug', '_layout.pug']
	}),
	rev
]), {
	pretty: true,
	_: require('lodash'),
	helpers: require('./lib/helpers'),
	assets: {
		hashes: null,
		prefix: '/dist/',
		url: function (path) {
			if (!this.hashes) this.hashes = JSON.parse(readFileSync(join(this.basedir, 'assets.json'), { encoding: 'utf-8' }))
			path = path.replace('dist/', '')
			const hash = this.hashes[path]
			return (hash && `${this.prefix}${path}?${hash}`) || `${this.prefix}${path}`
		}
	},
	siteSettings: { }
})

micropanel = new MergeTrees([
	micropanel,
	new ConfigReplace(
		micropanel, rev, {
			files: [ 'micro-panel/src/micro-panel.html' ],
			configPath: 'assets.json',
			patterns: [{
				match: /['"]micro-panel\.js['"]/g,
				replacement: (assets, match, url) => `"/dist/micro-panel/src/micro-panel.js?${assets['micro-panel/src/micro-panel.js']}"`
			}]
		}
	)
], { overwrite: true })

const all = new MergeTrees([
	bowerdeps, micropanel, scripts, sw, styles, icons, errPages
])

const compressExts = ['js', 'css', 'svg', 'html']

module.exports = process.env.SKIP_COMPRESSION ? all : new MergeTrees([
	all,
	new Zopfli(all, { extensions: compressExts }),
	new Brotli(all, { extensions: compressExts })
])
