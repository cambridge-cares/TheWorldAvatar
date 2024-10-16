/**
	Next.js can be configured through a next.config.js file in the
	root of your project directory (for example, by package.json). 
	
	next.config.js is a regular Node.js module, not a JSON file.
	It gets used by the Next.js server and build phases, and it's
	not included in the browser build.
**/

const nextConfig = {
	reactStrictMode: true,
	basePath: "", //default 
	assetPrefix: process.env.BASE_PATH ?? "",
	compiler: { removeConsole: false },
	env: {
		KEYCLOAK: process.env.KEYCLOAK ?? "false",
	}
};


export default nextConfig;