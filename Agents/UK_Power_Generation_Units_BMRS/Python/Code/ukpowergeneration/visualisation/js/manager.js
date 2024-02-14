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

	// Callback to execute after metadata is read.
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
		DT.clickEvents = true;
		DT.placenames = true;
		DT.popup = new mapboxgl.Popup({
			closeButton: false,
			closeOnClick: false
		});
	}

	/**
	 * Returns the current map element.
	 * 
	 * @returns Map element
	 */
	getMap() {
		return this._map;
	}

	/**
	 * Returns the current LayerHandler instance.
	 * 
	 * @returns LayerHandler instance.
	 */
	getLayerHandler() {
		return this._layerHandler;
	}

	/**
	 * Returns the current Registry instance.
	 * 
	 * @returns Registry instance.
	 */
	getRegistry() {
		return this._registry;
	}

	/**
     * Add a callback what will fire after a feature within the 
     * input MapBox layer has been selected.
     * 
     * @param {String} layerName MapBox layerID
     * @param {Function} callback function to execute 
     */
	addSelectionCallback(layerName, callback) {
		if(this._interactionHandler != null) {
       		this._interactionHandler.addSelectionCallback(layerName, callback);
		}
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
		let firstGroup = this._registry.getFirstGroup();
		return this.plotGroup(firstGroup, updateSelects);
	}

	/**
	 * Given an array of strings signifying the grouping, this method will
	 * read and display data from the corresponding directory as defined by
	 * each level's meta.json file.
	 * 
	 * @param {String[]} group group selection (e.g. ["scenario-0", "time-0"]) 
	 * @param {Boolean} updateSelects force dropdowns to match the group selection
	 * @param {Function} callback optional callback to run once group is plotted
	 */
	plotGroup(group, updateSelects = true, callback = null) {
		DT.currentFeature = null;
		this.goToDefaultPanel();

		// Remove previously added layers
		this._layerHandler.removeLayers();

		// Remove previously added sources
		this._sourceHandler.removeSources();

		// Remember the group we're currently plotting
		DT.currentGroup = group;

		// Get the metadata defining that group
		var groupMeta = this._registry.getGroup(group);

		// Get each data set and add it as a source
		var sourcePromises = [];

		var groupData = groupMeta["dataSets"];
		var groupDir = groupMeta["thisDirectory"];
		for(var i = 0; i < groupData.length; i++) {
			let sourcePromise = this._sourceHandler.addSource(groupDir, groupData[i]);
			sourcePromises.push(sourcePromise);
		}

		// Wait until all sources are loaded before adding layers
		return Promise.all(sourcePromises).then(() => {

			// Add layer(s) for each dataset
			for(var i = 0; i < groupData.length; i++) {
				this._layerHandler.addLayer(groupData[i]);
				
				// Register mouse interactions if enabled
				let clickable = groupData[i]["clickable"];
				if(clickable == null || clickable == true) {

					if(groupData[i]["cluster"] == true) {
						// If clustering was enabled, register logic for the cluster layer.
						this._interactionHandler.registerInteractions([
							groupData[i]["name"] + "_cluster", 
							groupData[i]["locationType"]
						]);
					}

					if(groupData[i]["locationType"] === "line") {
						// Register interactions slightly differently for line layers
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

			if(callback != null) callback();
		});
	}

	/**
	 * Adds a special sky effects layer and (if enabled) 3D terrain.
	 */
	addSkyAndTerrain() {
		if(this._layerHandler != null) {
			this._layerHandler.addSkyLayer();
			this._layerHandler.setSunPosition(
				(DT.terrain === "dark") ? "sunsetStart" : "solarNoon"
			);
			console.log("INFO: Added special Sky layer.");
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

		// Specify default options
		let defaultOptions = {
			container: containerName,
			style: "mapbox://styles/mapbox/light-v10?optimize=true",
			center: this._registry.globalMeta["defaultCenter"],
			zoom: this._registry.globalMeta["defaultZoom"],
			pitch: this._registry.globalMeta["defaultPitch"],
			bearing: this._registry.globalMeta["defaultBearing"]
		};

		// Create the map instance
		mapboxgl.accessToken = mapboxAPI;
		this._map = new mapboxgl.Map(defaultOptions);

		// Now that we have a map, do some initialisation of handlers
		this._sourceHandler = new SourceHandler(this._map, this._registry);
		this._layerHandler = new LayerHandler(this._map);
		this._panelHandler = new PanelHandler(this._map);
		this._timeseriesHandler = new TimeseriesHandler();

		this._interactionHandler = new InteractionHandler(
			this._map, 
			this._registry, 
			this._panelHandler,
			this._timeseriesHandler,
			this._layerHandler
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

		// Show linked files (if they've been set)
		if(null != this._registry.globalMeta["linkedFiles"])  {
			let rootDir = this._rootDirectories[this._currentRootDirName];
			this._panelHandler.showLinkedFiles(this._registry.globalMeta, rootDir);
		}

        this.storePanelDefault();
	}

	/**
	 * Stores the current state of the side panel as it's default state.
	 */
	storePanelDefault() {
		this._panelHandler.storeDefault();
	}

	/**
	 * Build the controls for the Camera, Terrain, and Layer Tree.
	 * 
	 * @param {function} treeCallback Optional callback to fire when tree selections change.
	 * @param {function} selectCallback Optional callback to fire when dropdown selections change.
	 */
	 showControls(treeCallback = null, selectCallback = null) {
		// Initialise map controls
		this._controlHandler.initialise(
			this._map, 
			this._registry,
			treeCallback
		);
		
		// Hide the building outlines provided by mapbox
		this._controlHandler.hideBuildings();
		
		let rootDir = this._rootDirectories[this._currentRootDirName];
		if(rootDir == null) {
			console.log("ERROR: Cannot locate root directory for key '" + rootDir + "'!");
			return;
		}
		rootDir = (rootDir.endsWith("/")) ? rootDir : rootDir + "/";

		this._controlHandler.showControls(this._rootDirectories, this._currentRootDirName, selectCallback);
	}

	/**
	 * Shows debugging info like mouse position.
	 */
	showDeveloperControls() {
		if(this._controlHandler != null) this._controlHandler.showDeveloperControls();
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
		try {
			this._controlHandler.onLayerGroupChange(control);
		} catch(error) {
			console.log(error, error.stack);
		}
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	onLayerChange(control) {
		try {
			this._controlHandler.onLayerChange(control);
		} catch(error) {
			console.log(error, error.stack);
		}
	}

	/**
	 * Pass-through.
	 * 
	 * @param {Element} control event source 
	 */
	changeTerrain(mode) {
		if(mode === DT.terrain) return;
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
	 * Adds a callback to fire once the user returns to the default side panel.
	 * 
	 * @param {Function} callback 
	 */
	setDefaultPanelCallback(callback) {
		this._defaultPanelCallback = callback;
	}

	/**
	 * Returns to the default state of the side panel
	 */
	goToDefaultPanel() {
		this._panelHandler.returnToDefault();
		if(this._defaultPanelCallback != null) {
			this._defaultPanelCallback();
		}
        console.log("RETURNED TO DEFAULT?");
	}
	
	/**
	 * Given a link to a markdown file, this method opens
	 * and renders the file in a new tab.
	 */
	openMarkdownLink(url) {
		let directories = null;
		let fileName = url;
		if(url.includes("/")) {
			let index = url.lastIndexOf("/");
			directories = url.substring(0, index);
			fileName = url.substring(index + 1, url.length);
		}

		$.get(url, function(contents) {
			var newContents = contents;

			if(directories != null) {
				// Assume image src in markdown files are relative to the MD file.
				// We need to update them in-memory to be relative to this HTML file.
				let regex = new RegExp(`src="(?!http)[^"]*`, `g`);
				let matches = [];

				let match;
				while ((match = regex.exec(contents)) !== null) {
					matches.push(match[0]);
				}

				matches.forEach(match => {
					let replacement = match.replace("src=\"", "");
					replacement = replacement.replace("./", "");
					replacement = "src=\"" + directories + "/" + replacement;
					newContents = newContents.replace(match, replacement);
				});
			}

			// Open the (potentially adjusted) markdown contents in a new document
			var newWindow = window.open("", fileName, "");
			newWindow.document.write(`
				<html>
					<head>
						<title>` + fileName + `</title>
						<meta charset="utf-8">
					</head>
					<body style="background-color: rgb(240, 240, 240); border-radius: 8px;">
						<div style="margin: 30px 90px; padding: 25px 50px; background-color: white;">
							` +  marked.parse(newContents) + `
						</div>
					</body>
				</html>
			`);
			newWindow.document.close();
		});
	}

	/**
	 * Given a link to a JSON file, this method opens
	 * and renders the file in a new tab.
	 */
	openJSONLink(url) {
		let directories = null;
		let fileName = url;
		if(url.includes("/")) {
			let index = url.lastIndexOf("/");
			directories = url.substring(0, index);
			fileName = url.substring(index + 1, url.length);
		}

		$.get(url, function(contents) {
			var newContents = contents;

			// Open the (potentially adjusted) JSON contents in a new document
			var newWindow = window.open("", fileName, "");
			newWindow.json = newContents;
			newWindow.document.write(`
				<html>
					<head>
						<title>` + fileName + `</title>
						<meta charset="utf-8">
						<link href="./css/framework/jsonview.bundle.css" rel="stylesheet"> 
						<script src='./js/framework/jsonview.bundle.js'></script>
					</head>
					<body style="background-color: rgb(240, 240, 240); border-radius: 8px;">
						<div id="container" style="margin: 30px 90px; padding: 25px 50px; background-color: white;">
						</div>
						<script>
							var tree = JsonView.renderJSON(window.json, document.getElementById("container"));
                			JsonView.expandChildren(tree);
							console.log("INFO: JSON tree has been rendered.");
						</script>
					</body>
				</html>
			`);
			newWindow.document.close();
		});
	}

	/**
	 * Pass through
	 * 
	 * @param {*} selectID 
	 * @param {*} selectValue 
	 */
	onGroupSelectChange(selectID, selectValue) {

		if(selectID === "root-dir-select") {
			// Change of root directory
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

	/**
	 * Enable (or disable) 3D terrain provided by MapBox.
	 * 
	 * @param {Boolean} enabled 3D terrain state.
	 */
	set3DTerrain(enabled) {
		if(this._sourceHandler != null) {
			this._sourceHandler.set3DTerrain(enabled);
		}
	}

	/**
	 * Enable (or disable) depth of field effect.
	 * 
	 * @param {Boolean} enabled depth of field state. 
	 */
	setTiltshift(enabled) {
		var tiltShiftComponent = document.getElementById("tiltShift");
		tiltShiftComponent.style.display = (enabled) ? "block" : "none";

		if(enabled) {
			var self = this;
			this._map.on("zoom", function() {
				self.#updateTiltShift();
			});
			this._map.on("pitch", function() {
				self.#updateTiltShift();
			});
			this.#updateTiltShift();
		}
	}

	/**
	 * Shows/hides place name labels supplied by MapBox.
	 * 
	 * @param {Boolean} enabled 
	 */
	setPlacenames(enabled) {
		if(enabled == null && DT.placenames != null) {
			enabled = DT.placenames;
		} else if(enabled == null) {
			return;
		}

		let ids = ["road-number-shield", "road-label", "road-intersection", "waterway-label", "natural-line-label",
		"natural-point-label", "water-line-label", "water-point-label", "poi-label", "airport-label", "settlement-subdivision-label",
		"settlement-minor-label", "settlement-major-label", "settlement-label", "state-label", "country-label", "road-oneway-arrow-blue", 
		"road-oneway-arrow-white", "transit-label"]

		ids.forEach(id => {
			if(this._map.getLayer(id) != null) {
				this._map.setLayoutProperty(
					id,
					"visibility",
					(enabled ? "visible" : "none")
				);
			} 
		});
		DT.placenames = enabled;
	}

	/**
	 * Updates the tiltshift effect based on the current zoom and pitch.
	 */
	#updateTiltShift() {
		var tiltShiftComponent = document.getElementById("tiltShift");

		if(tiltShiftComponent.style.display === "block") {
			var blurAmount = 5;

			var pitch = this._map.getPitch();
			var zoom = this._map.getZoom() / 20;
			var fractionPitched = zoom * (pitch / 90);

			tiltShiftComponent.style.backdropFilter = "blur(" + blurAmount + "px)";
			tiltShiftComponent.style.webkitBackdropFilter = "blur(" + blurAmount + "px)";

			var topStart = "black " + (5 * fractionPitched) + "%";
			var topEnd = "rgba(0, 0, 0, 0) " +  (60 * fractionPitched) + "%";
			var bottomStart = "rgba(0, 0, 0, 0) " + (100 - (15 * fractionPitched)) + "%";
			var bottomEnd ="rgba(0, 0, 0, 0.5) 100%";
		
			tiltShiftComponent.style.webkitMaskImage = "linear-gradient(" + topStart + ", " + topEnd + ", " + bottomStart +  ", " + bottomEnd + ")";
			tiltShiftComponent.style.maskImage = "linear-gradient(" + topStart + ", " + topEnd + ", " + bottomStart +  ", " + bottomEnd + ")";
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
		let groupName = group.join("/");
		let selectionsContainer = document.getElementById("selectionsContainer");
		let selects = selectionsContainer.querySelectorAll("select");

		for(var i = depth; i < selects.length; i++) {
			if(selects[i].id === "root-dir-select") continue;
			let selectOptions = selects.item(i).options;

			for(var j = 0; j < selectOptions.length; j++) {

				let match = selectOptions[j].value === groupName || groupName.startsWith(selectOptions[j].value + "/");
				if(match) {
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
	 * Hide/show all controls and overlays.
	 * 
	 * @param {Boolean} visibility desired state 
	 */
	hideAllControls(visibility) {
		var sidePanel = document.getElementById("sidePanel");
		var controlsContainer = document.getElementById("controlsContainer");

		sidePanel.style.display = (visibility) ? "block" : "none";
		controlsContainer.style.display = (visibility) ? "block" : "none";
		
		if(visibility) {
			if(sidePanel.classList.contains("collapsed")) {
				document.getElementById("map").style.width = "calc(100% - 28px)";
			} else {
				document.getElementById("map").style.width = "calc(100% - 500px)";
			}
		} else {
			if(sidePanel.classList.contains("large")) {
				this._panelHandler.toggleMode();
			}
			document.getElementById("map").style.width = "100%";
		}
		this._map.resize();
	}

}
// End of class.