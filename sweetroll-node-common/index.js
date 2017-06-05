const env = process.env
const jwtkey = env.SWEETROLL_SECRET || 'TESTKEY' // JWT signature private key. Make a long pseudorandom string in production

module.exports = {
	authentication (log, verify) {
		return async function (ctx, next) {
			const host = env.FAKE_HOST || ctx.request.host
			const token = ctx.cookies.get('Bearer') || ctx.request.header.Authorization
			let auth
			if (token) {
				try {
					auth = verify(token.replace('Bearer ', ''), jwtkey, { issuer: host })
					auth.sub = auth.sub.replace(/\/$/, '')
					log('auth success: %O', auth)
				} catch (err) {
					auth = undefined
					log('auth error: %O', err)
				}
			}
			ctx.auth = auth
			await next()
		}
	}
}
