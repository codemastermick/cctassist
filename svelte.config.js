import adapter from '@sveltejs/adapter-static'
import preprocess from "svelte-preprocess";

/** @type {import('@sveltejs/kit').Config} */
const config = {
	preprocess: preprocess(),

	kit: {
		adapter: adapter({
			pages: 'build',
			assets: 'build',
			fallback: 'index.html',
			precompress: false
		}),
		alias: {
			'$lib': './src/lib/',
			'$translations': './src/lib/translations',
		}
	}
};

export default config;
