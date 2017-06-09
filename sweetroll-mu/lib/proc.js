'use strict'

global.DataView = require('jdataview')
global.DOMParser = require('xmldom').DOMParser
const pify = require('pify')
const readMetadata = pify(require('exiv2').getImageTags)
const fileType = require('file-type')
const replaceExt = require('replace-ext')
const fs = require('mz/fs')
const basename = require('path').basename
const dirname = require('path').dirname
const tmp = require('tmp')
const Jimp = require('jimp')
const Vibrant = require('node-vibrant')
const ffmpeg = require('fluent-ffmpeg')
const imagemin = require('imagemin')
const pngquant = require('imagemin-pngquant')
const jpegoptim = require('imagemin-jpegoptim')
const mozjpeg = require('imagemin-mozjpeg')
const webp = require('imagemin-webp')
const sizeOf = require('image-size')
const murmurHash = require('murmurhash-native').murmurHash

const pngconf = () => ({ plugins: [
	pngquant({ quality: '85-95' }),
] })
const jpegconf = (q) => ({ plugins: [
	jpegoptim({ quality: 99 }), // removes metadata
	mozjpeg({ quality: q || 75 }),
] })
const webpconf = (q) => ({ plugins: [
	webp({ quality: q || 70 }),
] })
const webplosslessconf = () => ({ plugins: [
	webp({ nearLossless: 0 }),
] })

const ffprobe = (src) => {
	return new Promise((resolve, reject) =>
		ffmpeg.ffprobe(src, (err, data) => err ? reject(err) : resolve(data))
	)
}

const mp4 = (src) => {
	const path = tmp.tmpNameSync()
	return new Promise((resolve, reject) => {
		ffmpeg(src)
			.on('error', reject)
			.on('end', () => resolve(path))
			.audioCodec('aac')
			.videoCodec('libx264')
			.outputOptions(['-crf 25', '-pix_fmt yuv420p', '-profile:v baseline', '-level 3', '-b:a 96k'])
			.format('mp4')
			.renice(10)
			.save(path)
	})
}

const webm = (src) => {
	const path = tmp.tmpNameSync()
	return new Promise((resolve, reject) => {
		ffmpeg(src)
			.on('error', reject)
			.on('end', () => resolve(path))
			.audioCodec('opus')
			.videoCodec('libvpx-vp9')
			.outputOptions(['-crf 38', '-b:v 0', '-tile-columns 6', '-frame-parallel 1', '-speed 1', '-b:a 96k'])
			.format('webm')
			.renice(10)
			.save(path)
	})
}

const poster = (src) => {
	const path = tmp.tmpNameSync() + '.png'
	return new Promise((resolve, reject) => {
		ffmpeg(src)
			.on('error', reject)
			.on('end', () => resolve(path))
			.screenshots({
				count: 1,
				folder: dirname(path),
				filename: basename(path)
			})
	})
}

const aac = (src) => {
	const path = tmp.tmpNameSync()
	return new Promise((resolve, reject) => {
		ffmpeg(src)
			.on('error', reject)
			.on('end', () => resolve(path))
			.audioCodec('aac')
			.outputOptions(['-b:a 96k'])
			.format('mp4')
			.renice(10)
			.save(path)
	})
}

const opus = (src) => {
	const path = tmp.tmpNameSync()
	return new Promise((resolve, reject) => {
		ffmpeg(src)
			.on('error', reject)
			.on('end', () => resolve(path))
			.audioCodec('opus')
			.outputOptions(['-compression_level 9', '-b:a 96k'])
			.format('ogg')
			.renice(10)
			.save(path)
	})
}

module.exports = (origName, buf) => {
	const ft = fileType(buf)
	const name = murmurHash(buf, 'hex') + '_' + origName
	const origP = Promise.resolve({ name, body: buf, type: ft.mime })
	const addImageMeta = (o) => readMetadata(buf).then(meta => {
		// Avoid huge blobs and empty values
		for (const k of Object.keys(meta)) {
			if ( (meta[k].length > 128 && k !== 'Exif.Photo.UserComment' && k !== 'Exif.Image.Copyright' && k !== 'Exif.Image.Artist')
				|| meta[k].length === 0) {
				delete meta[k]
			}
		}
		o.meta = meta
		return o
	}).catch(e => {
		console.error('Could not extract metadata:', e)
		return o
	}).then(o =>
		Vibrant.from(buf).getPalette().then(palette => {
			o.palette = {}
			for (const k of Object.keys(palette)) {
				const item = palette[k]
				o.palette[k] = { color: item.getHex(), population: item.getPopulation() }
			}
			return o
		})
	).catch(e => {
		console.error('Could not find palette:', e)
		return o
	}).then(o =>
		Jimp.read(buf).then(j => {
			o.width = j.bitmap.width
			o.height = j.bitmap.height
			return new Promise((resolve, reject) =>
				j.resize(32, Jimp.AUTO).quality(100)
				.getBuffer(Jimp.MIME_JPEG, (err, val) => err ? reject(err) : resolve(val))
			).then(jpgbuf => imagemin.buffer(jpgbuf, webpconf(30))).then(webpbuf => {
				o.tiny_preview = 'data:image/webp;base64,' + webpbuf.toString('base64')
				return o
			})
		})
	).catch(e => {
		console.error('Could not make tiny preview / read width and height:', e)
		return o
	})

	if (ft.mime === 'image/png') {

		const pngP = imagemin.buffer(buf, pngconf())
			.then(newbuf => ({ name, body: newbuf, type: 'image/png' }))
		const webpP = imagemin.buffer(buf, webplosslessconf())
			.then(newbuf => ({ name: replaceExt(name, '.webp'), body: newbuf, type: 'image/webp' }))
		return Promise.all([pngP, webpP])
			.then(srcs => ({ source: srcs }))
			.then(addImageMeta)

	} else if (ft.mime === 'image/jpeg') {

		let quality = 65
		let saveOrig = false
		const size = sizeOf(buf)
		const dim = Math.max(size.width, size.height)
		if (dim >= 5000) {
			quality = 30
			saveOrig = true
		} else if (dim >= 4000) {
			quality = 35
			saveOrig = true
		} else if (dim >= 3000) {
			quality = 40
			saveOrig = true
		} else if (dim >= 2500) {
			quality = 50
		} else if (dim >= 2000) {
			quality = 60
		}
		const jpegP = imagemin.buffer(buf, jpegconf(quality))
			.then(newbuf => ({ name, body: newbuf, type: 'image/jpeg' }))
		const webpP = imagemin.buffer(buf, webpconf(quality - 5))
			.then(newbuf => ({ name: replaceExt(name, '.webp'), body: newbuf, type: 'image/webp' }))
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
			.then(addImageMeta)

	} else if (ft.mime === 'image/webp') {

		const jpegP = imagemin.buffer(buf, jpegconf())
			.then(newbuf => ({ name, body: newbuf, type: 'image/jpeg' }))
		return Promise.all([jpegP, origP])
			.then(srcs => ({ source: srcs }))
			.then(addImageMeta)

	} else if (ft.mime.startsWith('video')) {

		const srcPath = tmp.tmpNameSync()
		return fs.writeFile(srcPath, buf).then(() => {
			const webmP = webm(srcPath).then(path => ({ name: replaceExt(name, '.webm'), body: path, type: 'video/webm' }))
			const mp4P = mp4(srcPath).then(path => ({ name: replaceExt(name, '.mp4'), body: path, type: 'video/mp4' }))
			const posterP = poster(srcPath)
				.then(fs.readFile)
				.then(buf => imagemin.buffer(buf, jpegconf()))
				.then(buf => ({ name: replaceExt(name, '.poster.jpg'), body: buf, type: 'image/jpeg' }))
			return Promise.all([webmP, mp4P, posterP])
				.then(srcs => ({ source: srcs }))
				.then(o => ffprobe(srcPath).then(probe => {
					const vid = (probe.streams || []).filter(x => x.codec_type === 'video')[0]
					if (vid) {
						o.width = vid.width
						o.height = vid.height
					}
					const aud = (probe.streams || []).filter(x => x.codec_type === 'audio')[0]
					if (!aud) {
						o.controls = false
						o.loop = true
						o.autoplay = true
						o.muted = true
					}
					o.meta = (probe.format || {}).tags
					return o
				}))
		}).catch(e => {
			return fs.unlink(srcPath).then(() => { throw e })
		})

	} else if (ft.mime.startsWith('audio')) {

		const srcPath = tmp.tmpNameSync()
		return fs.writeFile(srcPath, buf).then(() => {
			const opusP = opus(srcPath).then(path => ({ name: replaceExt(name, '.opus'), body: path, type: 'audio/ogg' }))
			const aacP = aac(srcPath).then(path => ({ name: replaceExt(name, '.m4a'), body: path, type: 'audio/mp4' }))
			return Promise.all([opusP, aacP])
				.then(srcs => ({ source: srcs }))
				.then(o => ffprobe(srcPath).then(probe => {
					const aud = (probe.streams || []).filter(x => x.codec_type === 'audio')[0]
					o.meta = (probe.format || {}).tags || (aud || {}).tags
					return o
				}))
		}).catch(e => {
			return fs.unlink(srcPath).then(() => { throw e })
		})

	}

	return origP.then(src => ({ source: [src] }))
		.then(addImageMeta)
}
