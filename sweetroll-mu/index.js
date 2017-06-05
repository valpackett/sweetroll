'use strict'
process.chdir(__dirname)
require('dotenv').config()
const env = process.env
const log = require('debug')('sweetroll-mu')
const urlJoin = require('url-join')

function backendFs () {
	const fsRoot = env.FS_ROOT || '/tmp'
	const httpRoot = env.FS_URL || '/tmp'
	const fs = require('mz/fs')
	const pathJoin = require('path').join
	return async function (name, buf, _) {
		await fs.writeFile(pathJoin(fsRoot, name), buf)
		return urlJoin(httpRoot, name)
	}
}

function backendS3 () {
	const s3Bucket = env.S3_BUCKET
	const httpRoot = env.S3_URL
	if (!s3Bucket) throw new Error('You need to specify S3_BUCKET')
	if (!httpRoot) throw new Error('You need to specify S3_URL')
	const AWS = require('aws-sdk')
	const s3 = new AWS.S3({ params: { Bucket: s3Bucket } })
	return async function (name, buf, type) {
		const res = await s3.putObject({
			Key: name,
			Body: buf,
			ACL: 'public-read',
			ContentType: type,
			ContentDisposition: 'inline',
		}).promise()
		return urlJoin(httpRoot, name)
	}
}

let uploadBackend
switch ((env.UPLOAD_BACKEND || 'fs').toLowerCase()) {
	case 's3': uploadBackend = backendS3(); break
	default: uploadBackend = backendFs(); break
}

const proc = require('./lib/proc')
const getStream = require('get-stream')
const common = require('../sweetroll-node-common')
const Koa = require('koa')
const app = new Koa()
app.use(common.authentication(log, require('jsonwebtoken').verify))
app.use(require('koa-busboy')({}))
app.use(async (ctx, next) => {
	if (!ctx.auth) {
		ctx.response.status = 401
		ctx.response.set('WWW-Authenticate', 'Bearer')
		return next()
	}
	const file = ctx.request.files[0]
	const buf = await getStream.buffer(file)
	const results = await proc(file.filename, buf)
	for (const s of results.source) {
		s.src = await uploadBackend(s.name, s.buf)
		delete s.name
		delete s.buf
	}
	ctx.response.set('Location', results.source[0].src)
	ctx.response.type = 'json'
	ctx.response.body = JSON.stringify(results)
	return next()
})

const argv = require('minimist')(process.argv.slice(2))
if (argv.protocol === undefined || argv.protocol === 'http') {
	const port = argv.port || 3333
	app.listen(port, () => { log('listening on port', port) })
} else if (argv.protocol === 'activate') {
	app.listen({ fd: 3 }, () => { log('listening on fd 3') })
} else if (argv.protocol === 'unix') {
	const socket = argv.socket || 'app.sock'
	app.listen(socket, () => { log('listening on socket', socket) })
} else {
	console.error('Unknown protocol ' + argv.protocol)
}
