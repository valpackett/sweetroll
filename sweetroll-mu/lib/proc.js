'use strict'

global.DataView = require('jdataview')
global.DOMParser = require('xmldom').DOMParser
const ExifReader = require('exifreader')
const fileType = require('file-type')
const replaceExt = require('replace-ext')
//const ffmpeg = require('fluent-ffmpeg')
const imagemin = require('imagemin')
const pngquant = require('imagemin-pngquant')
const zopfli = require('imagemin-zopfli')
//const jpegoptim = require('imagemin-jpegoptim')
const mozjpeg = require('imagemin-mozjpeg')
const webp = require('imagemin-webp')

const pngconf = () => ({ plugins: [
	pngquant({ quality: '70-90' }),
	zopfli(),
] })
const jpegconf = () => ({ plugins: [
	//jpegoptim({ quality: 99 }), // removes metadata
	mozjpeg({ quality: 75 }),
] })
const webpconf = () => ({ plugins: [
	webp({ quality: 70 }),
] })
const webplosslessconf = () => ({ plugins: [
	webp({ lossless: true }),
] })

module.exports = (name, buf) => {
	const ft = fileType(buf)
	if (ft.mime === 'image/png') {

		const pngP = imagemin.buffer(buf, pngconf())
			.then(newbuf => ({ name, buf: newbuf, type: 'image/png' }))
		const webpP = imagemin.buffer(buf, webplosslessconf())
			.then(newbuf => ({ name: replaceExt(name, '.webp'), buf: newbuf, type: 'image/webp' }))
		return Promise.all([pngP, webpP])
			.then(srcs => ({ source: srcs }))

	} else if (ft.mime === 'image/jpeg') {

		let exif = null
		try {
			exif = ExifReader.load(buf)
		} catch (e) {
			console.error(e)
		}
		const jpegP = imagemin.buffer(buf, jpegconf())
			.then(newbuf => ({ name, buf: newbuf, type: 'image/jpeg' }))
		const webpP = imagemin.buffer(buf, webpconf())
			.then(newbuf => ({ name: replaceExt(name, '.webp'), buf: newbuf, type: 'image/webp' }))
		return Promise.all([jpegP, webpP])
			.then(srcs => ({ source: srcs, meta: exif }))

	} else if (ft.mime === 'image/webp') {

		const jpegP = imagemin.buffer(buf, jpegconf())
			.then(newbuf => ({ name, buf: newbuf, type: 'image/jpeg' }))
		return Promise.all([jpegP, Promise.resolve({ name, buf, type: 'image/webp' })])
			.then(srcs => ({ source: srcs }))

	}// else if (ft.mime.startsWith('video')) { }

	return Promise.resolve({ source: [{ name, buf }] })
}
