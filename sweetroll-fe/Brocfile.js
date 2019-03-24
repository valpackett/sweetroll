'use strict'

import { join } from 'path'
import { readFileSync } from 'fs'
import * as helpers from './src/helpers'
import Funnel from 'broccoli-funnel'
import MergeTrees from 'broccoli-merge-trees'
import AssetRev from 'broccoli-static-asset-rev'
import ConfigReplace from 'broccoli-config-replace'
import SVGStore from 'broccoli-svgstore'
import { SourceMapExtractor } from 'broccoli-source-map'
import Pug from 'broccoli-pug-render'
import PostCSS from 'broccoli-postcss'
import Concat from 'broccoli-concat'

const npmdeps = new Funnel('node_modules', {
	include: [
		'katex/dist/katex.css',
		'katex/dist/fonts/*',
		'@webcomponents/webcomponentsjs/*.js',
		'findandreplacedomtext/src/*.js',
		'svgxuse/*.min.js',
		'localforage/dist/localforage.js',
		'lazyload-image/lazyload-image.html',
		'indieweb-components/*.{html,js}',
		'highlight.js/styles/*.css',
	],
	exclude: [ '@webcomponents/webcomponentsjs/gulpfile.js' ],
})

let styles = new Concat(new MergeTrees([
	new Funnel('assets', { include: [ '*.css' ] }),
	new Funnel('node_modules', { include: [
		'sanitize.css/*.css', 'normalize-opentype.css/*.css'
	] }),
]), {
	outputFile: 'style.css',
	inputFiles: ['sanitize.css/sanitize.css', 'normalize-opentype.css/normalize-opentype.css', 'style.css']
})

styles = new SourceMapExtractor(new PostCSS(styles, {
	plugins: [
		{ module: require('postcss-nesting') },
		{ module: require('postcss-responsive-type') },
		{ module: require('postcss-flexbugs-fixes') },
		{ module: require('autoprefixer') },
		{ module: require('cssnano') },
	],
	from: 'assets/style.css',
	map: { inline: true, annotation: true }
}))

styles = new MergeTrees([styles, new Funnel('assets', { include: ['color.css'] })])

const icons = new SVGStore(
	new Funnel('node_modules/octicons/build/svg', {
		include: [
			'arrow-up', 'arrow-down', 'reply', 'megaphone', 'link',
			'star', 'info', 'bookmark', 'quote', 'lock',
			'device-camera', 'telescope', 'eye', 'desktop-download', 'paintcan'
		].map((x) => x + '.svg')
	}),
	{ outputFile: 'icons.svg' }
)

let micropanel = new Funnel('../micro-panel', {
	destDir: 'micro-panel',
	include: [ 'dist/*', 'icons/*', 'manifest.json' ]
})

let rev = new AssetRev([ npmdeps, micropanel, styles, icons ])

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

rev = new AssetRev([ npmdeps, micropanel, styles, icons, scripts ]) // TODO: append instead of recompute

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
			replacement: (assets, _) => `PRECACHED_ASSETS = ${JSON.stringify(
				Object.keys(assets)
					.filter(url => !url.includes('micro-panel') && !url.includes('katex') && !url.includes('highlight.js'))
					.map(url => `/dist/${url}?${assets[url]}`))}`
		}]
	}
)

const errPages = new Pug([new MergeTrees([
	new Funnel('views', {
		include: ['401.pug', '403.pug', '404.pug', '429.pug', '500.pug', '502.pug', '503.pug', '504.pug', 'offline.pug', '_layout.pug', '_media.pug', '_entry.pug', '_globals.pug', '_icons.pug']
	}),
	rev
])], {
	pretty: true,
	_: require('lodash'),
	qs: require('qs'),
	revHash: require('rev-hash'),
	helpers,
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

module.exports = new MergeTrees([
	npmdeps, micropanel, scripts, sw, styles, icons, errPages
])
