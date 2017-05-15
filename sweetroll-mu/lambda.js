'use strict'

const proc = require('./lib/proc')
const AWS = require('aws-sdk')
const Busboy = require('busboy')
const getStream = require('get-stream')
const urlJoin = require('url-join')
const jwt = require('jsonwebtoken')
const LambdaEnvVars = require('lambda-env-vars')
const encVars = new LambdaEnvVars.default()

exports.handler = (e, ctx, cb) => {
	encVars.getCustomDecryptedValue('SWEETROLL_SECRET').then(jwtkey => {
		let auth
		try {
			auth = jwt.verify(e.authorization.replace('Bearer ', ''), jwtkey, { })
		} catch (err) {
			auth = undefined
			return cb(err)
		}
		if (!auth) {
			return cb("Unauthorized")
		}
		const busboy = new Busboy({ headers: { 'content-type': e.contentType } })
		busboy.on('file', (fieldname, file, filename, encoding, mimetype) => {
			console.log(fieldname, filename, file)
			getStream.buffer(file).then(buf => {
				return proc(filename, buf)
			}).then(results => {
				console.log(results)
				const s3 = new AWS.S3({ params: { Bucket: process.env.S3_BUCKET } })
				return Promise.all(results.source.map(s => {
					return s3.putObject({
						Key: s.name,
						Body: s.buf,
						ACL: 'public-read',
						ContentType: s.type,
					}).promise().then(res => {
						s.src = urlJoin(process.env.S3_URL, s.name)
						delete s.name
						delete s.buf
						return s
					})
				})).then(srcs => {
					results.source = srcs
					cb(null, { location: results.source[0].src, body: results })
				})
			}).catch(cb)
		})
		busboy.on('error', cb)
		const buf = Buffer.from(e.body, 'base64')
		busboy.write(buf)
		busboy.end()
	}).catch(cb)
}
