var Cesium = require("cesium/Cesium");
/**
 * Add the tileset to the viewer and set its model matrix for geolocation
 * @param cesiumviewer    Cesium viewer object
 * @param cesiumtileset   filepath to tileset.json
 * @param x               latitude or x coordinate of a geolocation
 * @param y               longitude or y coordinate of a geolocation
 * @param z               elevation or z coordinate of a geolocation
 * @return                Cesium viewer primitive object
 */
export function addTileset(cesiumviewer, cesiumtileset, x, y, z = 0) {
    let position = Cesium.Cartesian3.fromDegrees(x, y, z);

    return cesiumviewer.scene.primitives.add(
        new Cesium.Cesium3DTileset({
            url: cesiumtileset,
            modelMatrix: Cesium.Transforms.eastNorthUpToFixedFrame(position),
        })
    );
}

/**
 * Add a glTF/glb model to the viewer and set its model matrix for geolocation
 * @param cesiumviewer    Cesium viewer object
 * @param modelfilepath   filepath to model
 * @param x               latitude or x coordinate of a geolocation
 * @param y               longitude or y coordinate of a geolocation
 * @param z               elevation or z coordinate of a geolocation
 * @return                Cesium viewer primitive object
 */
 export function addModel(cesiumviewer, modelfilepath, x, y, z = 0) {
    let position = Cesium.Cartesian3.fromDegrees(x, y, z);

    return cesiumviewer.entities.add({
        position: position,
        model : {				
            uri : modelfilepath
        },
    });
}

/**
 * Add a KML model to the viewer
 * @param cesiumviewer    Cesium viewer object
 * @param kmlfilepath     filepath to KML model
 * @return                Cesium viewer primitive object
 */
 export function addKml(cesiumviewer, kmlfilepath) {

    return cesiumviewer.dataSources.add(Cesium.KmlDataSource.load(kmlfilepath,
    {
      camera: cesiumviewer.scene.camera,
      canvas: cesiumviewer.scene.canvas,
      clampToGround: true,
    })
  );
}

/**
 * Add a WMS imagery layer to the viewer
 * @param cesiumviewer    Cesium viewer object
 * @param wms_url         url to WMTS imagery layer
 * @param wms_layer       name of WMTS imagery layer
 * @return                Cesium viewer primitive object
 */
 export function addWMSLayer(cesiumviewer, wms_url, wms_layer) {
    return cesiumviewer.imageryLayers.addImageryProvider(
        new Cesium.WebMapServiceImageryProvider({
          url: wms_url,
          layers: wms_layer,
          parameters: {
            transparent: true,
            format: "image/png",
          },
        })
      );
}

/**
 * Add a WMTS imagery layer to the viewer
 * @param cesiumviewer    Cesium viewer object
 * @param wmts_url        url to WMTS imagery layer
 * @param wmts_layer      name of WMTS imagery layer
 * @return                Cesium viewer primitive object
 */
 export function addWMTSLayer(cesiumviewer, wmts_url, wmts_layer) {
    return cesiumviewer.imageryLayers.addImageryProvider(
        new Cesium.WebMapTileServiceImageryProvider({
          url: wmts_url,
          layer: wmts_layer,
          style : 'default',
          format : 'image/jpeg',
          tileMatrixSetID : 'default028mm',
          // tileMatrixLabels : ['default028mm:0', 'default028mm:1', 'default028mm:2' ...],
          maximumLevel: 19,
        })
    );
}
