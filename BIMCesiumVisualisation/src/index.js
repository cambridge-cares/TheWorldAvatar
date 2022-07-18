import "../src/css/assetInfoBox.css"
import "../src/css/viewerElements.css"
var Cesium = require("cesium/Cesium");
import "cesium/Widgets/widgets.css";

// Your access token can be found at: https://cesium.com/ion/tokens.
// This is the default access token
Cesium.Ion.defaultAccessToken = 'your access token';

/* <----------------------------- Functions -----------------------------> */
/**
 * Add the tileset to the viewer and set its model matrix for geolocation
 * @param cesiumtileset   name of tileset.json file
 * @param x               latitude or x coordinate of a geolocation
 * @param y               longitude or y coordinate of a geolocation
 * @param z               elevation or z coordinate of a geolocation
 * @return                Cesium viewer primitive object
 */
function addTileset(cesiumtileset, x, y, z = 0) {
  let coordinates = Cesium.Cartesian3.fromDegrees(x, y, z);

  return viewer.scene.primitives.add(
    new Cesium.Cesium3DTileset({
      url: cesiumtileset,
      modelMatrix: Cesium.Transforms.eastNorthUpToFixedFrame(coordinates),
      debugShowBoundingVolume: false,
    })
  )
}

// Show or hide the ceiling depending on checkbox input
function hideFeature() {
  let checkBox = document.getElementById("toggleFeature");
  if (checkBox.checked == true) {
    tileset_ceiling.show = false;
  } else {
    tileset_ceiling.show = true;
  }
}

// Close the metadata infobox on click
function closeMetadata() {
  // Set transition time and effects for metadata infobox
  window.setTimeout(function () {
    metadataBox.style.transform = 'translate(100%, 0)';
    metadataBox.style.opacity = 0;
    metadataBox.style.visibility = 'hidden';
    metadataBox.style.transition = "visibility 0s 0.2s, opacity 0.2s ease-in, transform 0.2s ease-in";

    // Remove close button at same time
    closeElement.style.display = 'none';
  }, 0.2);
}

/**
 * Generates a formatted HTML string containing the picked feature's available metadata
 * @param metadata  the object referencing the picked feature's metadata stored in tileset
 * @return          HTML string
 */
function createMetadataHtml(metadata) {
  const propertyKeys = metadata.getPropertyIds();
  if (!Cesium.defined(propertyKeys)) {
    return `(No properties for ${title})<br>`;
  }
  // Formatting the information box into a table
  let html = "<div class='biminfobox-title'>"
  let htmlTable = "<table id='bim-infobox-table' class='biminfobox-content' style='border-collapse:collapse;'>";

  for (let i = 0; i < propertyKeys.length; i++) {
    const propertyKey = propertyKeys[i];
    const propertyValue = metadata.getProperty(propertyKey);
    if (propertyKey == "Asset Name") {
      html += `<h3>${propertyValue}</h3>`;
    } else {
      htmlTable += `<tr><td>${propertyKey} :</td> <td> ${propertyValue}</td></tr>`;
    }
  }
  html += "</div>" + htmlTable + "</table>";
  return html;
}

/*<----------------------------- Script Content -----------------------------> */
// Create a new viewer object
const viewer = new Cesium.Viewer("cesiumContainer");

/* Add new HTML elements */
// Creating a metadata overlay element when mouse moves over an asset
const promptOverlay = document.createElement("div");
viewer.container.appendChild(promptOverlay);
promptOverlay.className = "backdrop";

// Create a custom infobox element to present meta data when clicked on an asset
const metadataBox = document.createElement("div");
viewer.container.appendChild(metadataBox);
metadataBox.className = "biminfobox";

// Create a close button for the infobox
const closeElement = document.createElement("INPUT");
closeElement.id = "close";
closeElement.setAttribute("type", "button");
closeElement.value = "X";
closeElement.onclick = closeMetadata;
viewer.container.appendChild(closeElement);

// Add tilesets to the viewer
const bimTileset = './data/tileset_bim.json';
const ceilingTileset = './data/tileset_ceiling.json';
const tileset_bim = addTileset(bimTileset, 103.77398, 1.30411);
const tileset_ceiling = addTileset(ceilingTileset, 103.77398, 1.30411);
// Zoom to the tileset, with a small offset so that it is fully visible
const offset = new Cesium.HeadingPitchRange(
  Cesium.Math.toRadians(-45.0),
  Cesium.Math.toRadians(-45.0),
  80.0
);
viewer.zoomTo(tileset_bim, offset);

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
}, Cesium.ScreenSpaceEventType.MOUSE_MOVE);