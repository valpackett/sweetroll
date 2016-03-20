#!/usr/bin/env node
var svgstore = require('svgstore'), fs = require('fs')
var sprites = svgstore()
var folder = 'bower_components/Font-Awesome-SVG-PNG/black/svg/'
function add(x) { sprites = sprites.add(x, fs.readFileSync(folder + x + '.svg', 'utf8')) }

['arrow-up', 'arrow-down', 'reply', 'retweet', 'star', 'info-circle'].forEach(add)

fs.writeFileSync('./icons.svg', sprites)
