# BIM Cesium Visualisation

## Description
This repository is a web application for the visualisation of BIM/IFC models in Cesium. Based on this [example](https://github.com/CesiumGS/cesium-webpack-example), the app adopts the [CesiumJS](https://github.com/CesiumGS/cesium) library and [Webpack](https://github.com/webpack/webpack) bundling tool. The required `tileset.json` files in the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications can be obtained through the conversion tool in the `ifcto3Dtilesnext` module. 

# Instructions
## Usage
1. Run the conversion tool following the instructions in `ifcto3Dtilesnext` to get the required tilesets
    - SKIP this step if you have available tilesets
2. Register for a Cesium account to get your own access token and replace it in `src/index.js`
3. Place the tilesets and glTF/glb files in `src/data`
    - Following the glTF path reference of the tilesets generated from the `ifcto3Dtilesnext` tool, please place the glTF files in `src/data/gltf` 
    - Do note that if you are using tilesets from other sources, please place the glTF/glb files in a suitable path as per your tilesets
4. Navigate the `cmd` terminal to this directory and run 
    - *Pre-req:* [Download](https://nodejs.org/en/download/) and install Node.js and npm 
    ```
    npm install
    npm start
    ```
5. View the app on any browser, preferably Google Chrome at `localhost:3000`