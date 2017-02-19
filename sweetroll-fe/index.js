'use strict'
process.chdir(__dirname)
require('dotenv').config()
const log = require('debug')('sweetroll-fe:listen')
const app = require('./lib/app')
const argv = require('minimist')(process.argv.slice(2))
if (argv.procotol === undefined || argv.protocol === 'http') {
	const port = argv.port || 3000
	app.listen(port, () => { log('listening on port', port) })
} else if (argv.protocol === 'activate') {
	app.listen({ fd: 3 }, () => { log('listening on fd 3') })
} else if (argv.protocol === 'unix') {
	const socket = argv.socket || 'app.sock'
	app.listen(socket, () => { log('listening on socket', socket) })
} else {
	console.error('Unknown protocol ' + argv.protocol)
}
