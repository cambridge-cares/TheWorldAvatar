/**
 * Central controller for a single DigitalTwin visualisation.
 * 
 * @author Michael Hillman
 */
class DigitalTwinController {

	// MapBox map
	_map;

	// List of modules
	_modules = [];

	// MapContols instance
	_mapControls;

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

		// Initialise map controls
		this._mapControls = new MapControls(this._map, defaultCenter, defaultZoom);

		console.log("INFO: Map object has been initialised.");
		return this._map;
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

		// Add map controls to the document
		this._mapControls.buildTree(this._modules);
		document.getElementById("controlsParent").innerHTML = this._mapControls.controlHTML;
		this._mapControls.renderTree();
	}

	/**
	 * Fires when a group checkbox within the layer control is selected.
	 * 
	 * @param control - event source 
	 */
	 onGroupChange(control) {
		this._mapControls.onGroupChange(control);
	}

	/**
	 * Fires when a layer checkbox is selected.
	 * 
	 * @param control - event source 
	 */
	onLayerChange(control) {
		this._mapControls.onLayerChange(control);
	}

}