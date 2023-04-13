import { CapacitorConfig } from '@capacitor/cli';

const appId = 'com.antleredviking.cctassist';
const appName = 'CCTAssist';
const server = process.argv.includes('-hmr')
	? {
			url: 'http://10.0.0.53:5173', // always have http:// in url
			cleartext: true
	  }
	: {};
const webDir = 'build';

const config: CapacitorConfig = {
	appId,
	appName,
	webDir,
	server
};

if (process.argv.includes('-hmr'))
	console.log('WARNING: running capacitor with livereload config', config);

export default config;
