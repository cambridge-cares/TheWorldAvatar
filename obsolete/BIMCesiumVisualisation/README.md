# BIM Cesium Visualisation

## Description

IMPORTANT NOTE: This project has been superseded by the DTVF and the various IFC agents. Do not use - only for reference.

This repository is a web application for the visualisation of BIM/IFC models in Cesium. Based on this [example](https://github.com/CesiumGS/cesium-webpack-example), the app uses the [CesiumJS](https://github.com/CesiumGS/cesium) library and [Webpack](https://github.com/webpack/webpack) bundling tool. 

The required `tileset.json` files in the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications can be obtained through the conversion tool in the `ifcto3Dtilesnext` module. Please note that this module is only for IFC conversion, and is not intended for converting other data types.

# Instructions
## Usage
1. Run the conversion tool following the instructions in `ifcto3Dtilesnext` to get the required tilesets
    - SKIP this step if you have available tilesets
2. Register for a Cesium account to get your own access token and replace it in `src/js/index.js`
3. Place the tilesets and glTF/glb files in `src/data`
    - Following the glTF path reference of the tilesets generated from the `ifcto3Dtilesnext` tool, please place the glTF files in `src/data/gltf` file paths should be `src/data/tilesets.json` and `src/data/gltf/asset.gltf`
    - Do note that if you are using tilesets from other sources, please place the glTF/glb files in a suitable path as per your tilesets
4. Navigate `cmd` terminal to this directory (referred to as `<root>`)  and run 
    - *Pre-req:* [Download](https://nodejs.org/en/download/) and install Node.js and npm 
    ```
    npm install
    ```
5. Run `npm start` in `cmd` to start the web app
    - For optimal rendering performance, run `npm run demonstrator` instead
        - Do note that this has a longer start up time

## For Developers
### 1) Available scripts
- `npm start` - Starts a development server with a webpack build
- `npm run build` - Runs and saves an optimized webpack build for production
- `npm run demonstrator` - Starts a server with an optimized webpack build for demonstrations

### 2) Active Development
1. Run `npm start` at `<root>`
2. A web app should be deployed automatically in development mode at `localhost:3000`
    - Code changes are immediately reflected on the server
3. Visit [Cesium Sandcastle](https://sandcastle.cesium.com/) for sample codes to add more functionality
4. Add new code to `src/js/index.js` to test changes

**Georeferencing guide**
- Do note georeference information is lost from Revit/IFC, so do not attempt to place the coordinates in BIM
- You may used either Google Maps or Upload the file to their Cesium Ion Assets, and then get a rough estimate for longitude and latitude (Cesium uses CRS: EPSG4979)
- Adjustments will still be required for accurate geolocation

**Extra Documentation**

Further tutorials can be found in the `documentation` folder:
- Webpack

### 3) Deployment/ Production
1. Run `npm run build` at `<root>`
2. Test if the output is working
    - *Pre-req:* To install server: `npm install http-server -g`
    - In the `dist` folder, run `http-server -a localhost -p 3000 --cors=http://localhost:8080/`
        - `cors` parameter must be included to access data from local running server
    - View the web app in any browser, preferably Google Chrome, at `localhost:3000`
3. If the output works, deploy the `dist` folder via your production workflow    
