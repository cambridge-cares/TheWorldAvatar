const path = require('path');
const webpack = require('webpack');
// Copy folders to output
const CopyWebpackPlugin = require('copy-webpack-plugin');
// Path to the CesiumJS source code
const cesiumSource = 'node_modules/cesium/Source';
const cesiumWorkers = '../Build/Cesium/Workers';

module.exports = {
    entry: {
        app: './src/js/index.js'
    },
    amd: {
        // Enable webpack-friendly use of require in Cesium
        toUrlUndefined: true
    },
    resolve: {
        fallback: { "https": false, "zlib": false, "http": false, "url": false },
        alias: {
            cesium: path.resolve(__dirname, cesiumSource)
        },
        mainFiles: ['index', 'Cesium']
    },
    plugins: [        
        new CopyWebpackPlugin({
            patterns: [
                // Copy Cesium Assets, Widgets, and Workers to a static directory
                { from: path.join(cesiumSource, cesiumWorkers), to: 'Workers' },
                { from: path.join(cesiumSource, 'Assets'), to: 'Assets' },
                { from: path.join(cesiumSource, 'Widgets'), to: 'Widgets' },
                /* Copy tilesets and their glTF files into the output folder.
                Do not add these using Webpack's loaders or asset modules, as
                Webpack minifies and obfuscate the tilesets as per its default 
                behaviour for all files, and Cesium is unable to read them yet
                */
                { from: 'src/data', to: 'data' },
            ]
        }),
        new webpack.DefinePlugin({
            // Define relative base path in cesium for loading assets
            CESIUM_BASE_URL: JSON.stringify('')
        })
    ],
};