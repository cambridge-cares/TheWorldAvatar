/**
 * This class handles centralised controls for the Camera, Terrain, and Layer Tree.
 */
class DigitalTwinControls {

	// Layer tree handler
	_treeHandler;

	// HTML for controls
	_controlHTML = `
		<div id="controlContainer">
			<div id="cameraContainer">
				<p>Camera:</p>
				<a href="#" onclick="manager.changeCamera('bird')">Bird's Eye</a>
				<br>
				<a href="#" onclick="manager.changeCamera('pitch')">Pitched</a>
			</div>
			<div id="terrainContainer">
				<p>Terrain:</p>
				<input type="radio" name="terrain" id="light" onclick="manager.changeTerrain('light')" checked>
				<label for="light">Light</label>
				<br>
				<input type="radio" name="terrain" id="dark" onclick="manager.changeTerrain('dark')">
				<label for="dark">Dark</label>
				<br>
				<input type="radio" name="terrain" id="satellite" onclick="manager.changeTerrain('satellite')">
				<label for="satellite">Satellite</label>
				<br>
				<input type="radio" name="terrain" id="satellite-streets" onclick="manager.changeTerrain('satellite-streets')">
				<label for="satellite-streets">Satellite (with Streets)</label>
			</div>
			<div id="layerContainer">TREE-GOES-HERE</div>
		</div>
	`;

	// Default center position 
	_defaultCenter;

	// Default zoom level
	_defaultZoom;

	// MapBox map
	_map;

	// First time loading?
	_initialised = false;

	/**
	 * Initialise a new MapControls instance.
	 * 
	 * @param {MapBox Map} map MapBox map 
	 */
	constructor(map, defaultCenter, defaultZoom) {
		this._map = map;
		this._defaultCenter = defaultCenter;
		this._defaultZoom = defaultZoom;
	}

	/**
	 * Read the JSON defining the layer tree and build it.
	 * 
	 * @param {String} treeFile location of JSON defining layer tree.
	 */
	buildTree(treeFile, callback = null) {
		this._treeHandler = new DigitalTwinLayerTree(this._map, treeFile, callback);
	}

	/**
	 * Return HTML for controls.
	 */
	get controlHTML() {
		return this._controlHTML;
	}

	/**
	 * Pass-through for the group selection event logic.
	 * 
	 * @param {Element} control event source 
	 */
	onGroupChange(control) {
		this._treeHandler.onGroupChange(control);
   	}

   /**
	* Pass-through for the layer selection event logic.
	* 
	* @param {Element} control event source 
	*/
    onLayerChange(control) {
		this._treeHandler.onLayerChange(control);
    }

	/**
	 * After re-initialising the map, force the layer visibility to 
	 * match the existing selections in the layer tree.
	 */
	forceRefreshSelections() {
		if(!this._initialised) {
			this._initialised = true;
		} else {
			this._treeHandler.forceRefreshSelections();
		}
	}

	/**
	 * Change the underlying MapBox style.
	 * 
	 * @param {String} mode {"light", "dark", "satellite", "satellite-streets"}
	 */
	changeTerrain(mode) {
		if(mode === "light") {
			console.log("INFO: Changing terrain type to 'light'...");
			this._map.setStyle("mapbox://styles/mapbox/light-v10?optimize=true");
	
		} else if(mode === "dark") {
			console.log("INFO: Changing terrain type to 'dark'...");
			this._map.setStyle("mapbox://styles/mapbox/dark-v10?optimize=true");
	
		} else if(mode === "satellite") {
			console.log("INFO: Changing terrain type to 'satellite'...");
			this._map.setStyle("mapbox://styles/mapbox/satellite-v9?optimize=true");
	
		} else if(mode === "satellite-streets") {
			console.log("INFO: Changing terrain type to 'satellite-streets'...");
			this._map.setStyle("mapbox://styles/mapbox/satellite-streets-v11?optimize=true");
	
		} else {
			console.log("INFO: Unknown terrain type '" + mode + "', ignoring.");
		}

		DT.terrain = mode;
		console.log(DT.terrain);
	}

	/**
	 * Reset the camera to a default position.
	 * 
	 * @param {String} mode {"bird", "pitch"}
	 */
	changeCamera(mode) {
		if(mode === "bird") {
			console.log("INFO: Changing camera type to 'bird'...");
			this._map.flyTo({
				curve: 1.9,
				speed: 1.6,
				pitch: 0.0,
				bearing: 0.0,
				zoom: this._defaultZoom,
				center: this._defaultCenter
			});
	
		} else if(mode === "pitch") {
			console.log("INFO: Changing camera type to 'pitch'...");
			this._map.flyTo({
				curve: 1.9,
				speed: 1.6,
				pitch: 65,
				bearing: -30,
				zoom: this._defaultZoom,
				center: this._defaultCenter
			});
	
		} else {
			console.log("INFO: Unknown view type'" + mode + "', ignoring.");
		}
	}

}
// End of class.