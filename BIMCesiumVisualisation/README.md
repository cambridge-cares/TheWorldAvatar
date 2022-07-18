# BIM Cesium Visualisation

## Description
This repository is a web application for the visualisation of BIM/IFC models using the [CesiumJS](https://github.com/CesiumGS/cesium) library. The required `tileset.json` files in the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications can be obtained through a conversion workflow in the `ifcto3Dtilesnext` module. 

# Instructions
## Usage
1. Run the conversion tool following the instructions in `ifcto3Dtilesnext` to get the required tilesets
2. Register for a Cesium account to get your own access token and replace it in the `index.html` file
3. Navigate the `cmd` terminal to this directory with `index.html` and start a local server
    - Simplest method is to run the HTTP server on cmd terminal
        ```
        python -m http.server
        ```
    - If any errors are encountered, run `http-server -a localhost -p 8003 --cors=http://localhost:8080/` in CMD of folder
        - *Pre-req:* Download and install Node.js and npm from https://nodejs.org/en/download/
        - *Pre-req:* To install server: `npm install http-server -g`
        - `cors` parameter must be included to access data from local running server
4. View the results on any browser, preferably Google Chrome
    - If using python HTTP server, the address is at `localhost:8000`
    - If using Node.js HTTP server, the address is at `localhost:8003`

## For developers
In developing more Cesium features, please edit the index.html once you have tested the following:
1. Create your Cesium account for the access token
2. Find the latitude and longitude of your model (Cesium uses CRS: EPSG4979)
    - Georeference information is lost from Revit/IFC
    - You may used either Google Maps or Upload the file to their Cesium Ion Assets, and then get a rough estimate for longitude and latitude
    - Adjustments will still be required for accurate geolocation
3. Test the environment through setting up a basic skeleton of `index.html`
    - See key code below in `<script>` for a simple test with gltf file
    - Do not use `viewer.trackedEntity(entity)` to zoom to model as it locks to the model and disables some camera functionality 

    ```
    Cesium.Ion.defaultAccessToken = 'REPLACE with your access token';
    // Initialize the Cesium Viewer in the HTML element with the `cesiumContainer` ID.
    const viewer = new Cesium.Viewer('cesiumContainer'); 

    const position = Cesium.Cartesian3.fromDegrees(Longitude, Latitude, Height_usually 0); // Add coordinates here

    const orientation = Cesium.Transforms.headingPitchRollQuaternion(
        position,
        new Cesium.HeadingPitchRoll(1.6, 0, 0)
        );

    const entity = viewer.entities.add({
    position: position,
    orientation: orientation,
    model : {				
        uri : './data/output.gltf', //change this line
        scale: 100,
        },
    });

    // Zoom to model.
    viewer.zoomTo(entity);
    ```
4. Set up a local server to run the `index.html` with local files
    - Cesium Webpage cannot access any local files without a server due to cybersecurity concerns
    - Data must be stored in a subfolder of the index.html file
        - If they are both in the same folder, model is not uploaded for some reason
5. If test environment works, you can start developing the `index.html` with added functionality and tilesets
    - Check out Cesium Sandcastle for sample codes