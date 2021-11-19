/**
 * Central controller for a single DigitalTwin visualisation.
 */
class DigitalTwinManager {

	// MapBox map 
	_map;

	// Reads and stores metadata
	_registry;

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
		this._registry = new DataRegistry();
		this._controlHandler = new ControlHandler();
		
		// Create a new window namespace to let us set global variables
		window.DT = {};
		DT.terrain = "light";
		DT.currentGroup = [];
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
			console.log("ERROR: Cannot locate root directory for key '" + rootDir + "'!");
			return;
		}

		// Read all metadata
		let registry = this;
		this._registry.loadMetaData(rootDir, function() {
			// Read the layer tree specification
			let treeJSON = (rootDir.endsWith("/")) ? (rootDir + "tree.json") : (rootDir + "/tree.json");
			let treePromise = registry._controlHandler.readTreeFile(treeJSON);
			treePromise.then(() => {
				console.log("INFO: Layer tree specification has been read.");
				if(callback != null) callback();
			});
		});
	}

	/**
	 * Recurses depth-wise to find the first leaf group then plots that. Useful 
	 * as the default state of the map.
	 */
	plotFirstGroup(updateSelects = true) {
		this.plotGroup(this._registry.getFirstGroup(), updateSelects);
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * read and display data from the corresponding directory as defined by
	 * each level's meta.json file.
	 * 
	 * @param {String[]} group group selection (e.g. ["scenario-0", "time-0"]) 
	 * @param {Boolean} updateSelects force dropdowns to match the group selection
	 */
	plotGroup(group, updateSelects = true) {
		// Remove previously added layers
		this._layerHandler.removeLayers();

		// Remove previously added sources
		this._sourceHandler.removeSources();

		// Remember the group we're currently plotting
		DT.currentGroup = group;

		// Get the metadata defining that group
		var groupMeta = this._registry.getGroup(group);

		// Get each data set and add it as a source
		var groupData = groupMeta["dataSets"];
		var groupDir = groupMeta["thisDirectory"];
		for(var i = 0; i < groupData.length; i++) {
			this._sourceHandler.addSource(groupDir, groupData[i]);
		}

		// Add layer(s) for each dataset
		for(var i = 0; i < groupData.length; i++) {
			this._layerHandler.addLayer(groupData[i]);
			
			// Register interactions slightly differently for line layers
			if(groupData[i]["locationType"] === "line") {
				this._interactionHandler.registerInteractions([
					groupData[i]["name"] + "_clickable", 
					groupData[i]["locationType"]
				]);
			} else {
				this._interactionHandler.registerInteractions([
					groupData[i]["name"],
					groupData[i]["locationType"]
				]);
			}
		}

		// Rebuild the layer selection tree
		this._controlHandler.rebuildTree();

		// If a location was already selected, update the side panel
		if(DT.currentFeature != null) {
			this._interactionHandler.mouseClick(DT.currentFeature);
		}

		// Force selections to match this group
		if(updateSelects) {
			this.#forceSelects(group, 0);
		}
	}

	/**
	 * Recursively force the selection dropdowns to match the input
	 * group. Useful to ensure they represent the correct data after
	 * calling plotGroup manually.
	 * 
	 * @param {String[]} group group selection (e.g. ["scenario-0", "time-0"]) 
	 * @param {Integer} depth depth in recursive stack
	 */
	#forceSelects(group, depth) {
		let selectionsContainer = document.getElementById("selectionsContainer");
		let selects = selectionsContainer.querySelectorAll("select");

		for(var i = depth; i < selects.length; i++) {
			if(selects[i].id === "root-dir-select") continue;

			let selectOptions = selects.item(i).options;
			for(var j = 0; j < selectOptions.length; j++) {

				if(selectOptions[j].value.endsWith(group[depth])) {

					selects[i].value = selectOptions[j].value;
					if(depth != (group.length - 1)) {
						selects[i].onchange();
						this.#forceSelects(group, depth + 1);
					}
				}
			}
		}
	}

	/**
	 * Adds a special sky effects layer.
	 */
	setSkyDetails() {
		if(this._layerHandler != null) {
			this._layerHandler.addSkyLayer();
		}
	}

	/**
	 * Create a new MapBox map instance.
	 * 
	 * @param {String} containerName id of div to add map to.
	 * 
	 * @returns {MapBox map}
	 */
	createMap(containerName) {
		if(this._registry == null) {
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
			center: this._registry.globalMeta["defaultCenter"],
			zoom: this._registry.globalMeta["defaultZoom"],
			pitch: this._registry.globalMeta["defaultPitch"],
			bearing: this._registry.globalMeta["defaultBearing"]
		};

		// Create the map instance
		mapboxgl.accessToken = mapboxAPI;
		this._map = new mapboxgl.Map(defaultOptions);
		
		// Now that we have a map, do some initialisation of handlers
		this._sourceHandler = new SourceHandler(this._map);
		this._layerHandler = new LayerHandler(this._map);
		this._panelHandler = new PanelHandler(this._map);
		this._timeseriesHandler = new TimeseriesHandler();

		this._interactionHandler = new InteractionHandler(
			this._map, 
			this._registry, 
			this._panelHandler,
			this._timeseriesHandler
		);
		
		console.log("INFO: Map object has been initialised.");
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
	 showControls(treeCallback = null, selectCallback = null) {
		// Initialise map controls
		this._controlHandler.initialise(
			this._map, 
			this._registry,
			treeCallback
		);
		
		let rootDir = this._rootDirectories[this._currentRootDirName];
		if(rootDir == null) {
			console.log("ERROR: Cannot locate root directory for key '" + rootDir + "'!");
			return;
		}
		rootDir = (rootDir.endsWith("/")) ? rootDir : rootDir + "/";

		this._controlHandler.showControls(this._rootDirectories, this._currentRootDirName, selectCallback);
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
		this._layerHandler.setSunPosition(
			(mode === "dark") ? "sunsetStart" : "solarNoon"
		);
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
			let that = this;

			this.readMetadata(selectValue, function() {
				$("#selectionsContainer").children(":not(#rootSelectContainer)").remove();

				let selectString = that._controlHandler.buildDropdown(that._registry.meta);
				let newSelects = document.createElement("div");
				newSelects.innerHTML = selectString;
				document.getElementById("selectionsContainer").appendChild(newSelects);

				// Update map position
				that._map.jumpTo({
					center: that._registry.globalMeta["defaultCenter"],
					zoom: that._registry.globalMeta["defaultZoom"],
					pitch: that._registry.globalMeta["defaultPitch"],
					bearing: that._registry.globalMeta["defaultBearing"]
				});

				// Plot first leaf group by default
				that.plotFirstGroup();
			});
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