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

	// Handles MapBox interactions
	_interactionHandler;

	_timeseriesHandler;

	/**
	 * Initialisation.
	 */
	constructor() {
		// Create a new window namespace to let us set global variables
		window.DT = {};
		DT.terrain = "light";
		DT.currentAdditionals = [];
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
		this._dataRegistry = new DataRegistry();
		this._dataRegistry.loadMetaData(overallMeta, callback);
	}

	/**
	 * Reads and displays the Fixed Data sets as described in the previously
	 * read metadata files.
	 */
	plotFixedData() {
		this._sourceHandler.addFixedSources();

		let newLayerNames = this._layerHandler.addFixedLayers();
		newLayerNames.forEach(newLayerName => {
			this._interactionHandler.registerInteractions(newLayerName);
		})
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * read and display the corresponding Additional Data set as described
	 * in the previously read metadata files.
	 * 
	 * @param {string[]} groups group selection (e.g. ["scenario-0", "time-0"]) 
	 */
	plotAdditionalData(groups) {
		if(!DT.currentAdditionals.includes(groups)) {
			DT.currentAdditionals.push(groups);
        }

		this._sourceHandler.addAdditionalSources(groups);
		this._layerHandler.addAdditionalLayers(groups);
		
		this._controlHandler.rebuildTree();

		if(DT.currentFeature != null) {
			// A location was already selected, update the side panel
			this._interactionHandler.mouseClick(
				DT.currentFeature.layer["id"],
				DT.currentFeature
			);
		}
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * find the corresponding MapBox sources and layers, and remove them
	 * from the map.
	 * 
	 * @param {*} groups group selection (e.g. ["scenario-0", "time-0"]) 
	 */
	removeAdditionalData(groups) {
		if(DT.currentAdditionals.includes(groups)) {
            let index = DT.currentAdditionals.indexOf(groups);
            DT.currentAdditionals.splice(index, 1);
        }

		this._layerHandler.removeAdditionalLayers(groups);
		this._sourceHandler.removeAdditionalSources(groups);
		
		this._controlHandler.rebuildTree();
	}

	/**
	 * Will remove all Additional Data sets that are currently shown 
	 * on the map.
	 */
	 removeAllAdditionalData() {
		for(var i = (DT.currentAdditionals.length - 1); i >= 0; i--) {
            this.removeAdditionalData(DT.currentAdditionals[i]);
        }
	}

	restoreAllAdditionalData() {
		for(var i = (DT.currentAdditionals.length - 1); i >= 0; i--) {
            this.plotAdditionalData(DT.currentAdditionals[i]);
        }
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
		this._timeseriesHandler = new TimeseriesHandler();
		this._interactionHandler = new InteractionHandler(
			this._map, 
			this._dataRegistry, 
			this._panelHandler,
			this._timeseriesHandler
		);
		
		return this._map;
	}

	setPanelContent(title, content, legend, footer) {
		this._panelHandler.setTitle(title);
		this._panelHandler.setContent(content);
		this._panelHandler.setLegend(legend);
		this._panelHandler.setFooter(footer);

		this._panelHandler.storeDefault();
	}

	/**
	 * Build the controls for the Camera, Terrain, and Layer Tree.
	 * 
	 * @param {string} treeFile Location of JSON defining layer tree structure.
	 * @param {function} treeCallback Optional callback to fire when tree selections change.
	 * @param {function} treeCallback Optional callback to fire when dropdown selections change.
	 */
	 showControls(treeFile, treeCallback = null, selectCallback = null) {
		// Initialise map controls
		this._controlHandler = new ControlHandler(
			this._map, 
			this._dataRegistry,
			this._map.getCenter(), 
			this._map.getZoom(), 
			treeCallback
		);

		this._controlHandler.showControls(treeFile, selectCallback);
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
	 onLayerGroupChange(control) {
		this._controlHandler.onLayerGroupChange(control);
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

	goToDefaultPanel() {
		this._panelHandler.returnToDefault();
	}

	/**
	 * 
	 * @param {*} selectID 
	 * @param {*} selectValue 
	 */
	onGroupSelectChange(selectID, selectValue) {
		this._controlHandler.onGroupSelectChange(selectID, selectValue);
	}


	openTreeTab(event, tabName) {
		this._interactionHandler.openTreeTab(event, tabName);
	};


	updateTimeseries(setName) {
		this._timeseriesHandler.update(setName);
	}

}
// End of class.