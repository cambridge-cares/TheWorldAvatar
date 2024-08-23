import "../css/assetInfoBox.css"
import "../css/viewerElements.css"
var Cesium = require("cesium/Cesium");
import "cesium/Widgets/widgets.css";
import {
  addTileset,
  addModel,
  addKml,
  addWMSLayer,
  addWMTSLayer,
  computeCircle, getRoof
} from './functions/cesium_data_handler.js';
import {addDiv, addCloseButton, createMetadataHtml} from './functions/html-elements.js';

// Create a new viewer object
const viewer = new Cesium.Viewer("cesiumContainer",{
  timeline: false,
  animation: false,
  imageryProvider: new Cesium.OpenStreetMapImageryProvider({
    url : 'https://a.tile.openstreetmap.org/'
}),
  baseLayerPicker: false, 
  homeButton: false, 
  infoBox: false, 
  navigationHelpButton: false,
  projectionPicker: false,
  fullscreenButton: false,
  geocoder: false,
  sceneModePicker: false,
  selectionIndicator: false
});

// Set up coordinates and rotation
let coordinates = [103.77398, 1.30411, 0];
let axialRotation = [0, 0, 0]; // Add rotation to addTileset function when it is required

// Add tilesets to the viewer
const bimTileset = './data/tileset_bim.json';
const ceilingTileset = './data/tileset_ceiling.json';
const tileset_bim = addTileset(viewer, bimTileset, coordinates);
const tileset_ceiling = addTileset(viewer, ceilingTileset, coordinates);

/* 
// Adding gltf/glb models to the viewer
const gltfpath = './data/model.gltf';
const gltfmodel = addModel(viewer, gltfpath, coordinates);
*/

// Zoom to the tileset or gltf/glb model, with a small offset so that it is fully visible
const offset = new Cesium.HeadingPitchRange(
  Cesium.Math.toRadians(-45.0),
  Cesium.Math.toRadians(-45.0),
  80.0
);
// viewer.zoomTo(tileset_bim, offset);


// Adding KML models to the viewer
const kmlpath = './data/test_Tile_46_42_extruded.kml';
const kmlmodel =  addKml(viewer, kmlpath);

/*
// Adding WMS imagery layer to the viewer
const wmsurl = 'url';
const wmslayer = 'layername';
addWMSLayer(viewer,wmsurl,wmslayer);

// Adding WMTS imagery layer to the viewer
const wmtsurl = 'url';
const wmtslayer = 'layername';
addWMTSLayer(viewer, wmtsurl,wmtslayer); 
*/
// Zoom to the KML models, WMS or WMTS imagery
viewer.camera.flyTo({
  destination: Cesium.Cartesian3.fromDegrees(7.588534, 49.205952, 500),
});

// Creating a metadata overlay element when mouse moves over an asset
const promptOverlay = addDiv("backdrop");
// Create a custom infobox element to present meta data when clicked on an asset
const metadataBox = addDiv("biminfobox");
// Create a close button for the infobox
const closeElement = addCloseButton("close");

// Hides the ceiling and roof when checked
const checkBox1 = document.getElementById("toggleFeature");
checkBox1.onclick = () =>{
  if (checkBox1.checked == true) {
    tileset_ceiling.show = false;
  } else {
    tileset_ceiling.show = true;
  }
};

const checkBox2 = document.getElementById("solar");
var isSolar = false;
checkBox2.onclick = () =>{
  if (checkBox2.checked == true) {
    isSolar = true;
  } else {
    isSolar = false;
  }
};

const checkBox3 = document.getElementById("customsolar");
var drawSolar = false;
var arrayOfCartesian = [];
checkBox3.onclick = () =>{
  if (checkBox3.checked == true) {
    drawSolar = true;
  } else {
    drawSolar = false;
  }
};

// Append html elements to viewer
viewer.container.appendChild(promptOverlay);
viewer.container.appendChild(metadataBox);
viewer.container.appendChild(closeElement);

// Setting up for any event input on the cesium viewer screen
const handler = new Cesium.ScreenSpaceEventHandler(viewer.scene.canvas);

// Creating actions for mouse-over input
handler.setInputAction(function (movement) {
  let overlayText = "";

  // Create an object for the picked feature to retrieve the metadata stored in tileset
  const pickedFeature = viewer.scene.pick(movement.endPosition);
  const contentMetadata = pickedFeature?.content?.metadata;

  // If there is metadata for the asset
  if (Cesium.defined(contentMetadata)) {
    // display a prompt to click for more information
    promptOverlay.style.display = "block";
    promptOverlay.style.bottom = `${viewer.canvas.clientHeight - movement.endPosition.y}px`;
    promptOverlay.style.left = `${movement.endPosition.x}px`;
    promptOverlay.innerHTML = "Information available for " + contentMetadata.getProperty("Asset Name") + " - <b style ='font-size: 16px'>CLICK</b> for more details";

    // display the metadata information in a custom infoBox when clicked
    handler.setInputAction(function (movement) {
      // Display the infobox       
      metadataBox.style.display = "block";
      // Set transition time and effects
      window.setTimeout(function () {
        metadataBox.style.transform = 'translate(0, 0)';
        metadataBox.style.opacity = 1;
        metadataBox.style.visibility = 'visible';
        metadataBox.style.transition = "opacity 0.2s ease-out, transform 0.2s ease-out";

        // Display the close button at same time of transition
        closeElement.style.display = "block";
      }, 0.2);
      // Text input for metadata info box
      overlayText += createMetadataHtml(contentMetadata);
      metadataBox.innerHTML = overlayText;
    }, Cesium.ScreenSpaceEventType.LEFT_CLICK);

  } else {
    // do not display the mouse-over prompt
    promptOverlay.style.display = "none";

    // Add a empty left click handler when picked feature has no meta data
    // Without this empty handler, the UI for clicking becomes problematic
    handler.setInputAction(function (movement) {
    }, Cesium.ScreenSpaceEventType.LEFT_CLICK);
  }

    handler.setInputAction(function (movement) {

      if(drawSolar){

        // Find a way to get the position.
        var position = viewer.scene.pickPosition(movement.position);
        console.log(position);
        // var cartographic = Cesium.Ellipsoid.WGS84.cartesianToCartographic(position);
        // console.log(cartographic);
        arrayOfCartesian.push( position );
      } else {
        arrayOfCartesian = [];
        getRoof(viewer, pickedFeature, isSolar);
      }
    }, Cesium.ScreenSpaceEventType.LEFT_CLICK);


      handler.setInputAction(function (movement) {
        if(drawSolar) {
          // var getSelectorLocation = new Cesium.CallbackProperty(function () {
          //   return Cesium.Ellipsoid.WGS84.cartographicArrayToCartesianArray(arrayOfCartesian);
          // }, false);
          console.log(arrayOfCartesian);
          viewer.entities.add({
            polygon: {
              hierarchy: arrayOfCartesian,
              perPositionHeight: true,
              material: new Cesium.ImageMaterialProperty({
                image : './data/solarpanel.png',
                color: Cesium.Color.WHITE.withAlpha(1)
            }),
              zindex : 1
            }
          });
        }
      }, Cesium.ScreenSpaceEventType.RIGHT_CLICK);


}, Cesium.ScreenSpaceEventType.MOUSE_MOVE);



