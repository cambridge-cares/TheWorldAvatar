/**
 * Central controller for a single DigitalTwin visualisation.
 */
class DigitalTwinManager {

	// MapBox map 
	_map;

	// Reads and stores metadata
	_dataRegistry;

	// Reads data and adds MapBox sources
	_sourceHandler;

	// Generates and adds MapBox layers
	_layerHandler;

	// Handles the left-hand controls
	_controlHandler;

	// Handles the right-hand side panel
	_panelHandler;
	
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
	 * Scans for and reads the metadata that details the actual data within
	 * the expected visualisation data structure.
	 * 
	 * @param {string} overallMeta location of overall-meta.json file.
	 * @param {function} callback optional function to run once all metadata has been read.
	 */
	readMetadata(overallMeta, callback = null) {
		this._dataRegistry = new DigitalTwinDataRegistry();
		this._dataRegistry.loadMetaData(overallMeta, callback);
	}

	/**
	 * Reads and displays the Fixed Data sets as described in the previously
	 * read metadata files.
	 */
	plotFixedData() {
		this._sourceHandler.addFixedSources();
		this._layerHandler.addFixedLayers();
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * read and display the corresponding Additional Data set as described
	 * in the previously read metadata files.
	 * 
	 * @param {string[]} groups group selection (e.g. ["scenario-0", "time-0"]) 
	 */
	plotAdditionalData(groups) {
		this._sourceHandler.addAdditionalSources(groups);
		this._layerHandler.addAdditionalLayers(groups);
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * find the corresponding MapBox sources and layers, and remove them
	 * from the map.
	 * 
	 * @param {*} groups group selection (e.g. ["scenario-0", "time-0"]) 
	 */
	removeAdditionalData(groups) {
		this._sourceHandler.removeAdditionalSources(groups);
		
		// TODO - Remove layers
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
	createMap(containerName, apiKey, defaultCenter = [-2, 54.5], defaultZoom = 5) {
		// Specify default options
		let defaultOptions = {
			container: containerName,
			style: "mapbox://styles/mapbox/light-v10?optimize=true",
			center: this._dataRegistry.overallMeta["defaultCenter"],
			zoom: this._dataRegistry.overallMeta["defaultZoom"],
			pitch: 0.0,
			bearing: 0.0
		};

		// Create the map instance
		mapboxgl.accessToken = this._dataRegistry.overallMeta["apiKey"];
		this._map = new mapboxgl.Map(defaultOptions);
		console.log("INFO: Map object has been initialised.");
		
		// Now that we have a map, do some initialisation of handlers
		this._sourceHandler = new SourceHandler(this._dataRegistry, this._map);
		this._layerHandler = new LayerHandler(this._dataRegistry, this._map);
		this._panelHandler = new PanelHandler(this._map);

		// TEST - Hide the side panel for now - TEST
		this.togglePanelExpansion();

		return this._map;
	}

	/**
	 * Build the controls for the Camera, Terrain, and Layer Tree.
	 * 
	 * @param {String} treeFile Location of JSON defining layer tree structure.
	 * @param {Function} callback Optional callback to fire when layer selections change.
	 */
	 showControls(treeFile, callback = null) {
		// Initialise map controls
		this._controlHandler = new ControlHandler(
			this._map, 
			this._map.getCenter(), 
			this._map.getZoom(), 
			callback
		);

		this._controlHandler.showControls(treeFile);
	}

	/**
	 * Rebuilds the tree based on the tree specification file AND the current
	 * state of the map.
	 */
	rebuildTree() {
		this._controlHandler.rebuildTree();
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	 onGroupChange(control) {
		this._controlHandler.onGroupChange(control);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	onLayerChange(control) {
		this._controlHandler.onLayerChange(control);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	changeTerrain(mode) {
		this._controlHandler.changeTerrain(mode);
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	changeCamera(mode) {
		this._controlHandler.changeCamera(mode);
	}

	/**
	 * Opens the selected legend element
	 * 
	 * @param {MouseEvent} event mouse event
	 * @param {String} legendID id of selected legend
	 */
	openLegend(event, legendID) {
		//DT.sidePanelHandler.openLegend(event, legendID);
	}

	/**
	 * Toggle the expansion state of the side panel.
	 */
	togglePanelExpansion() {
		this._panelHandler.toggleExpansion();
	}

	/**
	 * Toggle the side panel's mode.
	 */
	togglePanelMode() {
		this._panelHandler.toggleMode();
	}


}
// End of class.