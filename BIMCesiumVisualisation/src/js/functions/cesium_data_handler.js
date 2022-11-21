var Cesium = require("cesium/Cesium");
/**
 * Add the tileset to the viewer and set its model matrix for geolocation
 * @param cesiumviewer    Cesium viewer object
 * @param cesiumtileset   filepath to tileset.json
* @param coordinate_array    Array of size 3 containing the x, y, and z-coordinates. These are the latitude, longitude, and elevation respectively.
 * @param rotation_array      Optional. Array of size 3 containing [roll, pitch, heading]. Defines as the rotation around the positive x, negative y, and negative z-axis by a given angle in radians.
 * @return                Cesium viewer primitive object
 */
export function addTileset(cesiumviewer, cesiumtileset, coordinate_array, rotation_array = [0, 0, 0]) {
  // Base model matrix
  let position = Cesium.Cartesian3.fromDegrees(coordinate_array[0], coordinate_array[1], coordinate_array[2]);
  let modelMatrix = Cesium.Transforms.eastNorthUpToFixedFrame(position);
  // Create a heading-pitch-roll object
  let hpr = new Cesium.HeadingPitchRoll(rotation_array[2], rotation_array[1], rotation_array[0]);
  // Create a rotation matrix
  let rotationMatrix = Cesium.Matrix3.fromHeadingPitchRoll(hpr, new Cesium.Matrix3());
  // And multiply the model matrix (position) by this rotation matrix
  Cesium.Matrix4.multiplyByMatrix3(modelMatrix, rotationMatrix, modelMatrix);

  return cesiumviewer.scene.primitives.add(
    new Cesium.Cesium3DTileset({
      url: cesiumtileset,
      modelMatrix: modelMatrix,
    })
  );
}

/**
 * Add a glTF/glb model to the viewer and set its model matrix for geolocation
 * @param cesiumviewer    Cesium viewer object
 * @param modelfilepath   filepath to model
 * @param coordinate_array    Array of size 3 containing the x, y, and z-coordinates. These are the latitude, longitude, and elevation respectively.
 * @return                Cesium viewer primitive object
 */
export function addModel(cesiumviewer, modelfilepath, coordinate_array) {
  let position = Cesium.Cartesian3.fromDegrees(coordinate_array[0], coordinate_array[1], coordinate_array[2]);

  return cesiumviewer.entities.add({
    position: position,
    model: {
      uri: modelfilepath
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
      style: 'default',
      format: 'image/jpeg',
      tileMatrixSetID: 'default028mm',
      // tileMatrixLabels : ['default028mm:0', 'default028mm:1', 'default028mm:2' ...],
      maximumLevel: 19,
    })
  );
}
