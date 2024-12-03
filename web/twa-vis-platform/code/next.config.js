/**
	Next.js can be configured through a next.config.js file in the
	root of your project directory (for example, by package.json). 
	
	next.config.js is a regular Node.js module, not a JSON file.
	It gets used by the Next.js server and build phases, and it's
	not included in the browser build.
**/

const nextConfig = {
	reactStrictMode: true,
	assetPrefix: process.env.ASSET_PREFIX ?? "",
	compiler: { removeConsole: false },
	images: {
		loader: 'custom',
		loaderFile: './image-loader.js',
	},
	env: {
		KEYCLOAK: process.env.KEYCLOAK ?? "false",
		ASSET_PREFIX: process.env.ASSET_PREFIX ?? "",
		MAPBOX_USER: process.env.MAPBOX_USER,
		MAPBOX_API_KEY: process.env.MAPBOX_API_KEY,
	}
};


export default nextConfig;