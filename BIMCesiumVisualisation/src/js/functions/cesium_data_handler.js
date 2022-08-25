import {Cartesian3, ColorMaterialProperty} from "cesium";
import {hierarchy} from "../../../../JPS/WebContent/javascript/d3.v4";

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
  ).then(function (dataSources){
        // parseElements(cesiumviewer, dataSources.entities)
    });
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

/**
 * compute shape of polylineVolume
 * @param radius
 * @return    object location
 */
function computeCircle(radius) {
    const positions = [];
    for (let i = 0; i < 360; i++) {
        const radians = Cesium.Math.toRadians(i);
        positions.push(
            new Cesium.Cartesian2(
                radius * Math.cos(radians),
                radius * Math.sin(radians)
            )
        );
    }
    return positions;
}

/**
 * Read data from KML to draw 3d geometric objects: line to polylineVolume, point to cylinder
 * @param cesiumviewer    Cesium viewer object
 * @param entities     data entity from kml
 */
function parseElements(cesiumviewer, entities) {
    var e;
    var pointCount = 0;
    var lineCount = 0;
    var values = entities.values;
    // console.dir(values); // debug the array
    var arrayPositions = [];
    for (var i = 0; i < values.length; i++) {
        e = values[i];
        var data = e.kml.extendedData;
        var width = 0;
        if (Cesium.defined(data)) {
            console.log("value=", data.sewWidth.value);
            if (data.sewWidth.value != null){ //get width from kml, if the data is empty, set a fake value to it
                width = data.sewWidth.value/100;
            }else{
                width = 0.5;
            }
        }

        if (Cesium.defined(e.position)) {
            // Placemark with Point geometry
            // pointCount++;
            var pointPosition = e.position;
            cesiumviewer.entities.add({
                position: pointPosition,
                cylinder: {
                    length: 5.0,
                    topRadius: width,
                    bottomRadius: width,
                    material: Cesium.Color.RED.withAlpha(0.5),
                    outline: true,
                    outlineColor: Cesium.Color.RED,
                },
            })
        } else if (Cesium.defined(e.polyline)) {
            // Placemark with LineString geometry
            // lineCount++;
            var linePosition = e.polyline.positions;
            // console.dir("position1:" + linePosition.getValue(Cesium.JulianDate.now()));
            cesiumviewer.entities.add({
                polylineVolume:{
                    positions : linePosition,
                    shape : computeCircle(width),
                    material: Cesium.Color.GREEN.withAlpha(0.5),
                    outline: true,
                    outlineColor: Cesium.Color.GREEN,
                },
            })
        } else if (Cesium.defined(e.polygon)) {
            // Placemark with Polygon geometry
        }
        // check for other conditions
    }


    // console.dir("point:" + pointCount);
    // console.dir("line:" + lineCount);
}

export function getRoof (cesiumviewer, entities, isSolar) {
    var id = entities.id.id; //depends on construction of kml in exporter

    if(id.includes("Roof")){
        var roofGeo =  entities.id.polygon;
        // console.dir(roofGeo.hierarchy.valueOf());
        if(isSolar){
            addTexture(roofGeo, id);
        }else{
            removeTexture(roofGeo, id);
        }
    }
}

var changeMap = new Map();
function addTexture(polygon, id){


    if(! changeMap.has(id)){
        changeMap.set(id, polygon.material);
    }
    polygon.material = './data/solarpanel.png';
}

function removeTexture(polygon, id){
    if(changeMap.has(id)){
        polygon.material = changeMap.get(id);
        changeMap.delete(id);
    }
}


