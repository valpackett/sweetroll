'use strict'

global.DataView = require('jdataview')
global.DOMParser = require('xmldom').DOMParser
const readMetadata = require('pify')(require('exiv2').getImageTags)
const fileType = require('file-type')
const replaceExt = require('replace-ext')
//const ffmpeg = require('fluent-ffmpeg')
const imagemin = require('imagemin')
const pngquant = require('imagemin-pngquant')
//const jpegoptim = require('imagemin-jpegoptim')
const mozjpeg = require('imagemin-mozjpeg')
const webp = require('imagemin-webp')
const sizeOf = require('image-size')
const murmurHash = require('murmurhash-native').murmurHash

const pngconf = () => ({ plugins: [
	pngquant({ quality: '85-95' }),
] })
const jpegconf = (q) => ({ plugins: [
	//jpegoptim({ quality: 99 }), // removes metadata
	mozjpeg({ quality: q || 75 }),
] })
const webpconf = (q) => ({ plugins: [
	webp({ quality: q || 70 }),
] })
const webplosslessconf = () => ({ plugins: [
	webp({ nearLossless: 0 }),
] })

module.exports = (origName, buf) => {
	const ft = fileType(buf)
	const name = murmurHash(buf, 'hex') + '_' + origName
	const origP = Promise.resolve({ name, buf, type: ft.mime })
	const addMetadata = (o) => readMetadata(buf).then(meta => {
		// Avoid huge blobs and empty values
		for (const k of Object.keys(meta)) {
			if ( (meta[k].length > 128 && k !== 'Exif.Photo.UserComment' && k !== 'Exif.Image.Copyright' && k !== 'Exif.Image.Artist')
				|| meta[k].length === 0) {
				delete meta[k]
			}
		}
		o.meta = meta
		return o
	})

	if (ft.mime === 'image/png') {

		const pngP = imagemin.buffer(buf, pngconf())
			.then(newbuf => ({ name, buf: newbuf, type: 'image/png' }))
		const webpP = imagemin.buffer(buf, webplosslessconf())
			.then(newbuf => ({ name: replaceExt(name, '.webp'), buf: newbuf, type: 'image/webp' }))
		return Promise.all([pngP, webpP])
			.then(srcs => ({ source: srcs }))
			.then(addMetadata)

	} else if (ft.mime === 'image/jpeg') {

		let quality = 70
		let saveOrig = false
		const size = sizeOf(buf)
		const dim = Math.max(size.width, size.height)
		if (dim >= 5500) {
			quality = 40
			saveOrig = true
		} else if (dim >= 4000) {
			quality = 45
			saveOrig = true
		} else if (dim >= 3000) {
			quality = 50
			saveOrig = true
		} else if (dim >= 2500) {
			quality = 60
		} else if (dim >= 2000) {
			quality = 65
		}
		const jpegP = imagemin.buffer(buf, jpegconf(quality))
			.then(newbuf => ({ name, buf: newbuf, type: 'image/jpeg' }))
		const webpP = imagemin.buffer(buf, webpconf(quality - 5))
			.then(newbuf => ({ name: replaceExt(name, '.webp'), buf: newbuf, type: 'image/webp' }))
		const ps = [jpegP, webpP]
		if (saveOrig) {
			ps.push(origP.then(x => {
				x.original = true
				x.name = 'orig_' + x.name
				return x
			}))
		}
		return Promise.all(ps)
			.then(srcs => ({ source: srcs }))
			.then(addMetadata)

	} else if (ft.mime === 'image/webp') {

		const jpegP = imagemin.buffer(buf, jpegconf())
			.then(newbuf => ({ name, buf: newbuf, type: 'image/jpeg' }))
		return Promise.all([jpegP, origP])
			.then(srcs => ({ source: srcs }))
			.then(addMetadata)

	}// else if (ft.mime.startsWith('video')) { }

	return origP.then(src => ({ source: [src] }))
		.then(addMetadata)
}
