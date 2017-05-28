'use strict'
process.chdir(__dirname)
require('dotenv').config()

const env = process.env
const fsRoot = env.UPLOAD_ROOT_FS || '/tmp'
const httpRoot = env.UPLOAD_ROOT_HTTP || '/tmp'

const proc = require('./lib/proc')
const fs = require('mz/fs')
const pathJoin = require('path').join
const urlJoin = require('url-join')
const getStream = require('get-stream')
const Koa = require('koa')
const app = new Koa()
app.use(require('koa-busboy')({}))
app.use(async (ctx, next) => {
	const file = ctx.request.files[0]
	const buf = await getStream.buffer(file)
	const results = await proc(file.filename, buf)
	for (const s of results.source) {
		await fs.writeFile(pathJoin(fsRoot, s.name), s.buf)
		s.src = urlJoin(httpRoot, s.name)
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
	app.listen(port, () => { console.log('listening on port', port) })
} else if (argv.protocol === 'activate') {
	app.listen({ fd: 3 }, () => { console.log('listening on fd 3') })
} else if (argv.protocol === 'unix') {
	const socket = argv.socket || 'app.sock'
	app.listen(socket, () => { console.log('listening on socket', socket) })
} else {
	console.error('Unknown protocol ' + argv.protocol)
}
