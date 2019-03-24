import { dirname } from 'path';
import debug from 'debug';
import minimist from 'minimist';
import app from './src/app';

const __dirname = dirname(new URL(import.meta.url).pathname);
process.chdir(__dirname)

const log = debug('sweetroll-fe:listen')
const argv = minimist(process.argv.slice(2))

if (argv.protocol === undefined || argv.protocol === 'http') {
	const port = argv.port || 3300
	app.listen(port, () => { log('listening on port', port) })
} else if (argv.protocol === 'activate') {
	app.listen({ fd: 3 }, () => { log('listening on fd 3') })
} else if (argv.protocol === 'unix') {
	const socket = argv.socket || 'app.sock'
	app.listen(socket, () => { log('listening on socket', socket) })
} else {
	console.error('Unknown protocol ' + argv.protocol)
}
