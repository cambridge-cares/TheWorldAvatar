/**
 * This Javascript provides common utilities for controlling MapBox map instances.
 * It is also capable of generating HTML controls for the map's style and camera; 
 * these can be embedded into other visualisations to ensure consistency across
 * different aspects of the UK Digital Twin.
 * 
 * Note that this script needs to be run in an environment/on a page that also
 * has the MapBox-GL JS library, and mapbox-controls.css stylesheet loaded.
 * 
 * Author: Michael Hillman (mdhillman<@>cmclinnovations.com)
 */

// HTML template for map controls
const controlHTML = `
	<div id="controlContainer">
		<div id="cameraContainer">
			<p>Camera:</p>
			<a href="#" onclick="changeCamera('bird')">Bird's Eye</a>
			<br>
			<a href="#" onclick="changeCamera('pitch')">Pitched</a>
		</div>
		<div id="terrainContainer">
			<p>Terrain:</p>
			<input type="radio" name="terrain" id="light" onclick="changeTerrain('light')" checked>
			<label for="light">Light</label>
			<br>
			<input type="radio" name="terrain" id="satellite" onclick="changeTerrain('satellite')">
			<label for="satellite">Satellite</label>
			<br>
			<input type="radio" name="terrain" id="satellite-streets" onclick="changeTerrain('satellite-streets')">
			<label for="satellite-streets">Satellite (with Streets)</label>
		</div>
		<div id="layerContainer">
			<p>Layers:</p>
		</div>
	</div>
`;

// Default parameters for the Bird's Eye camera
var defaultBird = {
	curve: 1.9,
	speed: 1.6,
	zoom: 5,
	pitch: 0.0,
	bearing: 0.0,
	center: [-2, 54.5]
};

// Default parameters for the Pitched camera
var defaultPitch = {
	curve: 1.9,
	speed: 1.6,
	zoom: 7,
	pitch: 65,
	bearing: -30,
	center: [-1.5, 52.5]
};

// Default options that should be used when initialising
// a new MapBox map instance
var defaultOptions = {
	container: "map",
	style: "mapbox://styles/mapbox/light-v10?optimize=true",
	center: [-2, 54.5],
	zoom: 5,
	pitch: 0.0,
	bearing: 0.0
};

// The maximum blur, in pixels, when the tilt-shift effect is fully on.
const tiltShiftMaxBlur = 3;

// Cached MapBox map instance
var map = null;

// Call back to fire after camera change
var cameraCallback = null;

// Call back to fire after terrain change
var terrainCallback = null;

// Call back to fire after layer toggle
var layerCallback = null;

// Ditionary of registered layer names to IDs
var registeredLayers = {};

/**
* Overrides the default settings for the bird camera option.
*
* @param newDefaults - new default options for bird camera
*/
function overrideDefaultBird(newDefaults) {
	defaultBird = newDefaults;
}

/**
* Overrides the default settings for the pitch camera option.
*
* @param newDefaults - new default options for pitch camera
*/
function overrideDefaultPitch(newDefaults) {
	defaultPitch = newDefaults;
}

/**
* Overrides the default mapbox settings for the map.
*
* @param newDefaults - new default options for map
*/
function overrideDefaultMap(newDefaults) {
	defaultOptions = newDefaults;
}


/**
 * Map options to be used for new maps.
 * 
 * @returns Default map options
 */
function getDefaultMapOptions() {
	return defaultOptions;
}

/**
 * Returns as HTML string containing the map controls that can be injected into 
 * the calling visualisation page.
 * 
 * @returns HTML string for map controls 
 */
function getControls() {
	return controlHTML;
}

/**
 * Initialises the map controls with default view and terrain (Bird's Eye and Light).
 * 
 * @param myMap - MapBox map instance
 * @param myCameraCallback - callback to fire after camera change
 * @param myTerrainCallback - callback to fire after terrain change
 * @param myLayerCallback - callback to fire after layer change, will be called with
 * 							layerID and selection state arguments (if null is passed a default will be used)
 */
function setup(myMap, myCameraCallback, myTerrainCallback, myLayerCallback) {
	map = myMap;
	cameraCallback = myCameraCallback;
	terrainCallback = myTerrainCallback;
	layerCallback = myLayerCallback;

	// Enable MapBox's own controls
	map.addControl(new mapboxgl.NavigationControl());

	if(layerCallback == null) {
		console.log("INFO: No layer callback passed, using default implementation.");

		layerCallback = function(layerID, selectionState) {
			try {
				map.setLayoutProperty(
					layerID,
					"visibility",
					(selectionState ? "visible" : "none")
				);
			} catch(err) {
				console.log("ERROR: Could not toggle '" + layerID + "', it may have no initial 'visibility' layout property?");
			}
		}
	}
}

/**
 * Refresh the terrain and sky settings after the map's style is changed.
 */
function refresh() {
	// Add terrain
	map.addSource('terrain-source', {
		'type': 'raster-dem',
		'url': 'mapbox://mapbox.mapbox-terrain-dem-v1',
		'tileSize': 512,
		'maxzoom': 14
	});

	map.setTerrain({ 
		'source': 'terrain-source',
		'exaggeration': 2
	});

	// Add the sky 
	map.addLayer({
		'id': 'sky',
		'type': 'sky',
		'paint': {
			'sky-type': 'atmosphere',
			'sky-atmosphere-color': '#6687eb',
			'sky-atmosphere-sun': [0.0, 0.0],
			'sky-atmosphere-sun-intensity': 15
		}
	});

	console.log("INFO: Terrain and sky layers have been added.");
}

/**
 * 
 * @param layerName - unique user facing layer name
 * @param layerIDs - array of layerIDs
 * @param groupName - optional name to group layer under
 * @param selected - default selection state
 */
function registerLayer(layerName, layerIDs, groupName, selected) {
	if(typeof(map) === "undefined" || map == null) {
		throw new Error("MapBox instance has not been set, run initialise() function!");
	}

	if(groupName == null) {
		groupName = "null";
	}

	// Register the group
	if(!(groupName in registeredLayers)) {
		registeredLayers[groupName] = {};
	}

	// Register the layers
	var layers = registeredLayers[groupName];
	layers[layerName] =  {
		"ids": layerIDs,
		"selected": selected
	};
}

/**
 * Build the checkbox tree for selectable groups and layers.
 * 
 * @param type - type of tree ("radio", "checkbox")
 */
function buildLayerTree(type) {
	var htmlString = `<p>Layers:</p><ul class="checktree">`;

	for(groupName in registeredLayers) {
		
		// Build HTML string for layer selections
		var groupLayers = registeredLayers[groupName];
		var selectedLayers = 0;
		var layerHTML = "";

		if(Object.keys(groupLayers).length > 0) {
			layerHTML += "<ul>";

			for(layerName in groupLayers) {
				if(groupName == "null") {
					layerHTML += `<li class="layerCheckNoGroup">`;
				} else {
					layerHTML += `<li class="layerCheckGroup">`;
				}

				if(groupLayers[layerName]["selected"]) {
					selectedLayers += 1;
					layerHTML += `<input type="` + type + `" onclick="onLayerChange(this);" name="` + groupName + `" id="` + layerName + `" checked>`;
				} else {
					layerHTML += `<input type="` + type + `" onclick="onLayerChange(this);" name="` + groupName + `" id="` + layerName + `">`;
				}

				layerHTML += `
					<label for="` + layerName + `">` + layerName + `</label>
					</li>
				`;		
			}
			layerHTML += "</ul>";
		}

		// Build HTML string for group selection (unless null)
		var groupHTML = "";

		if(groupName != "null") {
			groupHTML += `<li class="groupCheck">`;

			if(selectedLayers == Object.keys(groupLayers).length) {
				groupHTML += `<input type="` + type + `" onclick="onGroupChange(this);" id="` + groupName + `" checked>`;
			} else {
				groupHTML += `<input type="` + type + `" onclick="onGroupChange(this);" id="` + groupName + `">`;
			}
			groupHTML += `<label for="` + groupName + `">` + groupName + `</label>`;
		}

		// Bring it all together
		htmlString += groupHTML;
		htmlString += layerHTML;
		htmlString += "</li>";
	}

	htmlString += "</ul>";
	document.getElementById("layerContainer").innerHTML = htmlString;
}

/**
 * Fires when a group checkbox within the layer control is selected.
 * 
 * @param checkbox - event source 
 */
function onGroupChange(checkbox) {
	// Update the selection state of layers in this group
	var layers = registeredLayers[checkbox.id];

	for(layerName in layers) {
		layers[layerName]["selected"] = checkbox.checked;
		var ids = layers[layerName]["ids"];

		ids.forEach((layerID) => {
			console.log("INFO: Layer '" + layerID + "' now has visibility state: " + layers[layerName]["selected"]);
			layerCallback(layerID, checkbox.checked);
		});
	}
}

/**
 * Fires when a layer checkbox is selected.
 * 
 * @param checkbox - event source 
 */
function onLayerChange(checkbox) {
	var layers =  registeredLayers[checkbox.name];
	layers[checkbox.id]["selected"] = checkbox.checked;
	var ids = layers[checkbox.id]["ids"];

	ids.forEach((layerID) => {
		console.log("INFO: Layer '" + layerID + "' now has visibility state: " + layers[layerName]["selected"]);
		layerCallback(layerID, checkbox.checked);
	});
}

/**
 * Adds support for the Tilt Shift feature.
 * 
 * Note that this WILL override any existing functions tied to the 
 * map.on('move') event that MapBox provides.
 */
function addTiltShiftSupport() {
	if(typeof(map) === "undefined" || map == null) {
		throw new Error("MapBox instance has not been set, run initialise() function!");
	}

	// Check for the tiltShift div.
	var tiltShiftDiv = document.getElementById("tiltShift");
	if(tiltShiftDiv == null) {
		throw new Error("Cannot find div with id of 'tiltShift', will not enable effects!");
	}

	// Add photo effects and update on map movement
	addPhotoEffects();
	map.on('move', function () {
		addPhotoEffects();
	});
}

/**
 * Changes the terrain type of the current map.
 * 
 * @param terrainType - 'light', 'satellite', or 'satellite-street'
 */
function changeTerrain(terrainType) {
	if(typeof(map) === "undefined" || map == null) {
		throw new Error("MapBox instance has not been set, run initialise() function!");
	}

	if(terrainType === "light") {
		console.log("Changing terrain type to 'light'...");
		map.setStyle("mapbox://styles/mapbox/light-v10?optimize=true");

	} else if(terrainType === "satellite") {
		console.log("Changing terrain type to 'satellite'...");
		map.setStyle("mapbox://styles/mapbox/satellite-v9?optimize=true");

	} else if(terrainType === "satellite-streets") {
		console.log("Changing terrain type to 'satellite-streets'...");
		map.setStyle("mapbox://styles/mapbox/satellite-streets-v11?optimize=true");

	} else {
		console.log("Unknown terrain type '" + terrainType + "', ignoring.");
	}

	// Fire the callback (if present)
	if(terrainCallback != null) {
		terrainCallback(terrainType);
	}
}

/**
 * Resets the camera to one of the default settings.
 * 
 * @param viewType - 'bird' or 'pitch'
 */
function changeCamera(viewType) {
	if(typeof(map) === "undefined" || map == null) {
		throw new Error("MapBox instance has not been set, run initialise() function!");
	}

	if(viewType === "bird") {
		console.log("Changing camera type to 'bird'...");
		map.flyTo(defaultBird);

	} else if(viewType === "pitch") {
		console.log("Changing camera type to 'pitch'...");
		map.flyTo(defaultPitch);

	} else {
		console.log("Unknown view type'" + viewType + "', ignoring.");
	}

	// Fire the callback (if present)
	if(cameraCallback != null) {
		cameraCallback(viewType);
	}
}

/**
 * 
 */
function addPhotoEffects() {
	var pitch = map.getPitch();
	var pitchNormalized = pitch / 85;

	var bearing = map.getBearing() + 180;
	var zoom = map.getZoom() - 4;
	var fractionZoomedOut = 1 - (zoom / 18);
	var fractionPitched = (pitch / 80);
	var fractionPitchedBackwards = Math.max(1 - (pitch / 80), 0);

	var tiltShiftBackdropFilter = 'blur(' + ((tiltShiftMaxBlur * easeInCubic(fractionPitched)) * fractionZoomedOut) + 'px)';
	var tiltShiftGradientBlackPoint = (75 + (25 * easeOutCubic(fractionPitchedBackwards)));

	// Bug out if the tiltShift container cannot be found
	var tiltShiftDiv = document.getElementById("tiltShift");
	if(tiltShiftDiv == null) return;

	tiltShiftDiv.style.backdropFilter = tiltShiftBackdropFilter;
	tiltShiftDiv.style.webkitBackdropFilter = tiltShiftBackdropFilter;

	// this needs to be styled in two different ways to support the most browsers
	tiltShiftDiv.style.webkitMaskImage = '-webkit-gradient(linear, left bottom, left top, from(black), color-stop(5%, black), color-stop(45%, rgba(0, 0, 0, 0)), color-stop(55%, rgba(0, 0, 0, 0)), color-stop(' + tiltShiftGradientBlackPoint + '%, black), to(black))';
	tiltShiftDiv.style.maskImage = 'linear-gradient(0deg, black 0%, black 5%, rgba(0, 0, 0, 0) 45%, rgba(0, 0, 0, 0) 55%, black ' + tiltShiftGradientBlackPoint + '%)';
	console.log("Tiltshift applied");
}

function easeInCubic(x) {
	return x * x * x;
}

function easeOutCubic(x) {
	return 1 - Math.pow(1 - x, 3);
}

function easeInExpo(x) {
	return x === 0 ? 0 : Math.pow(2, 10 * x - 10);
}

function easeOutExpo(x) {
	return x === 1 ? 1 : 1 - Math.pow(2, -10 * x);
}

function degreesToRadians(degrees) {
	var pi = Math.PI;
	return degrees * (pi / 180);
}