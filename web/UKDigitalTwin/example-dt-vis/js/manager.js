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

	// Handles timeseries display
	_timeseriesHandler;

	// All possible root directories.
	_rootDirectories = [];

	// Current root directory name.
	_currentRootDirName;

	//
	_metaCallback;

	/**
	 * Initialisation.
	 */
	constructor() {
		this._dataRegistry = new DataRegistry();

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
     * Register multpiple possible root directories.
     * 
     * @param {dictionary} directories 
     */
	registerDirectories(directories) {
		this._rootDirectories = directories;
    }

	/**
	 * Scans for and reads the metadata that details the actual data within
	 * the expected visualisation data structure.
	 * 
	 * @param {string} rootDir name of the root directory containing all data
	 * @param {function} callback optional function to run once all metadata has been read.
	 */
	readMetadata(rootDirName, callback = null) {
		this._currentRootDirName = rootDirName;
		this._metaCallback = callback;

		let rootDir = this._rootDirectories[rootDirName];
		if(rootDir == null) {
			console.log("looking for " + rootDirName);
			console.log(this._rootDirectories);
			
			console.log("ERROR: Cannot locate root directory for key '" + rootDir + "'!");
			return;
		}

		this._dataRegistry.loadMetaData(rootDir, callback);
	}

	/**
	 * Reads and displays the Fixed Data sets as described in the previously
	 * read metadata files.
	 */
	plotFixedData() {
		this._sourceHandler.addFixedSources();

		let newLayers = this._layerHandler.addFixedLayers();
		newLayers.forEach(layer => {
			this._interactionHandler.registerInteractions(layer);
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
		let newLayers = this._layerHandler.addAdditionalLayers(groups);
		newLayers.forEach(layer => {
			this._interactionHandler.registerInteractions(layer);
		})
		
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
		console.log("REMOVED ADDITIONAL?");
	}

	/**
	 * Will replot any previously plotted Additional Data sets.
	 */
	restoreAllAdditionalData() {
		for(var i = (DT.currentAdditionals.length - 1); i >= 0; i--) {
            this.plotAdditionalData(DT.currentAdditionals[i]);
        }
		console.log("RESTORE ADDITIONAL?");
	}
	
	/**
	 * Create a new MapBox map instance.
	 * 
	 * @param {String} containerName id of div to add map to.
	 * 
	 * @returns {MapBox map}
	 */
	createMap(containerName) {
		if(this._dataRegistry == null) {
			console.log("ERROR: Cannot create map until metadata has been initialised!");
			return;
		}

		// If there was a previously selected terrain
		let terrainURL = null;
		switch(DT.terrain) {
			default:
				terrainURL = "mapbox://styles/mapbox/light-v10?optimize=true";
				break;
			case "dark":
				terrainURL = "mapbox://styles/mapbox/dark-v10?optimize=true";
				break;
			case "satellite":
				terrainURL = "mapbox://styles/mapbox/satellite-v9?optimize=true";
				break;
			case "satellite-streets":
				terrainURL = "mapbox://styles/mapbox/satellite-streets-v11?optimize=true";
				break;
		}

		// Specify default options
		let defaultOptions = {
			container: containerName,
			style: terrainURL,
			center: this._dataRegistry.overallMeta["defaultCenter"],
			zoom: this._dataRegistry.overallMeta["defaultZoom"],
			pitch: this._dataRegistry.overallMeta["defaultPitch"],
			bearing: this._dataRegistry.overallMeta["defaultBearing"]
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

	/**
	 * Sets the content of the right hand side panel.
	 * 
	 * @param {string} title 
	 * @param {string} content 
	 * @param {string} legend 
	 * @param {string} footer 
	 */
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
		
		let rootDir = this._rootDirectories[this._currentRootDirName];
		if(rootDir == null) {
			console.log("ERROR: Cannot locate root directory for key '" + rootDir + "'!");
			return;
		}
		rootDir = (rootDir.endsWith("/")) ? rootDir : rootDir + "/";

		let fullTreeFile = rootDir + treeFile;
		this._controlHandler.showControls(fullTreeFile, this._rootDirectories, this._currentRootDirName, selectCallback);
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

	/**
	 * Returns to the default state of the side panel
	 */
	goToDefaultPanel() {
		this._panelHandler.returnToDefault();
	}

	/**
	 * Pass through
	 * 
	 * @param {*} selectID 
	 * @param {*} selectValue 
	 */
	onGroupSelectChange(selectID, selectValue) {
		if(selectID == "root-dir-select") {
			this.readMetadata(selectValue, this._metaCallback);
		} else {
			this._controlHandler.onGroupSelectChange(selectID, selectValue);
		}
	}

	/**
	 * Pass through
	 * 
	 * @param {*} event 
	 * @param {*} tabName 
	 */
	openTreeTab(event, tabName) {
		this._interactionHandler.openTreeTab(event, tabName);
	};

	/**
	 * Pass through
	 * 
	 * @param {*} setName 
	 */
	updateTimeseries(setName) {
		this._timeseriesHandler.update(setName);
	}

	/**
	 * Fly to the input location.
	 * 
	 * @param {*} lon 
	 * @param {*} lat 
	 */
	zoomTo(lon, lat) {
		console.log("Zooming to " + lon + ", " + lat);
		this._map.flyTo({
			center: [lon, lat],
			essential: true,
			zoom: 18,
			bearing: 0,
			speed: 0.75
		});
	}

}
// End of class.