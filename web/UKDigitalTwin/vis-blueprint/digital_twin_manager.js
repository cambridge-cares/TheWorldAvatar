/**
 * Central controller for a single DigitalTwin visualisation.
 */
class DigitalTwinManager {

	// MapBox map
	_map;

	// MapBox popup
	_popup;

	// List of modules
	_modules = [];

	// MapContols instance
	_mapControls;

	// Has the map been loaded at least once
	_initialised = false;

	/**
	 * Initialisation.
	 */
	constructor() {
		// Create a new window namespace to let us set global variables
		window.DT = {};
		DT.terrain = "light";

		DT.popup = new mapboxgl.Popup({
			closeButton: false,
			closeOnClick: false
		});
	}
	
	/**
	 * Create a new MapBox map instance.
	 * 
	 * @param {String} containerName id of div to add map to.
	 * @param {String} apiKey MapBox API key.
	 * @param {Number[]} defaultCenter default center position.
	 * @param {Number} defaultZoom default zoom value.
	 * 
	 * @returns {MapBox map}
	 */
	createMap(containerName, apiKey, defaultCenter=[-2, 54.5], defaultZoom=5) {
		// Specify default options
		let defaultOptions = {
			container: containerName,
			style: "mapbox://styles/mapbox/light-v10?optimize=true",
			center: defaultCenter,
			zoom: defaultZoom,
			pitch: 0.0,
			bearing: 0.0
		};

		// Create the map instance
		mapboxgl.accessToken = apiKey;
		this._map = new mapboxgl.Map(defaultOptions);

		// Create popup
		this._popup = new mapboxgl.Popup({
			closeButton: false,
			closeOnClick: false
		});
			
		// Initialise map controls
		this._mapControls = new DigitalTwinControls(this._map, defaultCenter, defaultZoom);
		console.log("INFO: Map object has been initialised.");
		return this._map;
	}

	/**
	 * Build the controls for the Camera, Terrain, and Layer Tree.
	 * 
	 * @param {String} treeFile Location of JSON defining layer tree structure.
	 * @param {Function} callback Optional callback to fire when layer selections change.
	 */
	buildControls(treeFile, callback = null) {
		document.getElementById("controlsParent").innerHTML = this._mapControls.controlHTML;
		this._mapControls.buildTree(treeFile, callback);
	}

	/**
	 * Add a DigitalTwinModule for handling.
	 * 
	 * @param {DigitalTwinModule} module module to add.
	 */
	addModule(module) {
		let allowed = (module instanceof DigitalTwinModule);
		if(!allowed) {
			console.log("ERROR: Cannot add module that is not a DigitalTwinModule instance!");
		} else {
			module.setMap(this._map);
			this._modules.push(module);
			console.log("INFO: A new '" + module.name + "' module has been added.");
		}
	}

	/**
	 * Loads all modules that have been added, making the map ready for display.
	 */
	loadModules() {
		this._modules.forEach(dtModule => {
			dtModule.addSources();
			dtModule.addLayers();
		});

		if(!this._initialised) {
			this._initialised = true;
		} else {
			DT.treeHandler.forceRefreshSelections();
		}
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	 onGroupChange(control) {
		DT.treeHandler.onGroupChange(control);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	onLayerChange(control) {
		DT.treeHandler.onLayerChange(control);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	changeTerrain(mode) {
		DT.treeHandler.changeTerrain(mode);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	changeCamera(mode) {
		DT.treeHandler.changeCamera(mode);
	}

}
// End of class.