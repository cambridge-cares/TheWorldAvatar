
class DigitalTwinControls {

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
			<div id="layerContainer">
				<p>Layers:</p>
			</div>
		</div>
	`;

	// Default center position 
	_defaultCenter;

	// Default zoom level
	_defaultZoom;

	// MapBox map
	_map;

	// Layer headers to enforce radio groups
	_radioHeaders = [];

	// Tree data
	_treeData;

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
	 * Return HTML for controls.
	 */
	get controlHTML() {
		return this._controlHTML;
	}

	/**
	 * Any layers under the input heading will be forced to use RadioButton controls.
	 * 
	 * @param {String} heading heading name 
	 */
	addRadioHeading(heading) {
		this._radioHeaders.push(heading);
	}

	/**
	 * Show or hide a single layer on the map.
	 * 
	 * @param {String} layerName MapBox layer name.
	 * @param {boolean} visible desired visibility.
	 */
	#toggleLayer(layerName, visible) {
		try {
			this._map.setLayoutProperty(
				layerName,
				"visibility",
				(visible ? "visible" : "none")
			);
			console.log("INFO: The '" + layerName + "' now has visibility '" + visible + "'.");
		} catch(err) {
			console.log("WARN: Could not toggle '" + layerName + "', it may have no initial 'visibility' layout property?");
		}
	}

	/**
	 * Combines the registered layer groups from all modules into single tree like data structure.
	 * 
	 * @param {DigitalTwinModule[]} modules modules loaded in DigitalTwinController
	 */
	buildTree(modules) {
		// Final tree structure
		this._treeData = {};

		modules.forEach(mod => {
	
			for(let [heading, groups] of Object.entries(mod.layerGroups)) {
				let headingEntry = this._treeData[heading];
				if(headingEntry == null) {
					headingEntry = {};
				} 
				
				for(let [name, values] of Object.entries(groups)) {
					let groupEntry = headingEntry[name];
					if(groupEntry == null) {
						groupEntry = {
							"name": name,
							"enabled": values["enabled"],
							"layers": []
						};
					}

					let newLayers = groupEntry["layers"].concat(values["layers"]);
					groupEntry["layers"] = newLayers;
					headingEntry[name] = groupEntry;
				}
				this._treeData[heading] = headingEntry;
			}
		});
	}

	/**
	 * Render tree for HTML view.
	 * 
	 * @param {Object<String, Object>} treeData tree data structure
	 */
	renderTree() {
		var htmlString = `<p>Layers:</p><ul class="checktree">`;
		htmlString += "<ul class='groupList'>"	

		for(let [heading, values1] of Object.entries(this._treeData)) {

			// Add an item for the heading, unless it's "null"
			if(heading != "null") {
				htmlString += "<li>";
				htmlString += "<input type='checkbox' onclick='manager.onGroupChange(this);' id='" + heading + "'>";
				htmlString += "<label for='" + heading + "'>" + heading + "</label>";
				htmlString += "</li>";

				htmlString += "<ul class='layerList'>";
			} else {
				htmlString += "<ul class='layerList listNoIndent'>";
			}

			// Get type of control to use for layers in this heading
			let type = (this._radioHeaders.includes(heading)) ? "radio" : "checkbox";

			// For each layer set under this heading
			for(let [name, values2] of Object.entries(values1)) {
				htmlString += (heading === "null") ? "<li class='listNoIndent'>" : "<li>";

				let checked = this._treeData[heading][name]["enabled"];
				if(checked) {
					htmlString += "<input type='" + type + "' onclick='manager.onLayerChange(this);' id='" + name + "' name='" + heading + "' checked>";
				} else {
					htmlString += "<input type='" + type + "' onclick='manager.onLayerChange(this);' id='" + name + "' name='" + heading + "'>";
				}
			
				htmlString += "<label for='" + name + "'>" + name + "</label>";
				htmlString += "</li>";
			}
			htmlString += "</ul>"
		}
		htmlString += "</ul>";

		// Add to the document
		document.getElementById("layerContainer").innerHTML = htmlString;

		// Update group states based on default selections
		this.#updateGroupStates(null);
	}

	/**
	 * Fires when a group checkbox within the layer control is selected.
	 * 
	 * @param control - event source 
	 */
	onGroupChange(control) {
		// Update the selection state of layers in this group
		let groupName = control.id;
		let groupLayers = this._treeData[groupName];

		for(let [name, values] of Object.entries(groupLayers)) {
			let layerIDs = values["layers"];

			if(this._radioHeaders.includes(groupName)) {
				// Don't change the "enabled" variable for each layer here,
				// in this case we want to remember it.

				if(control.checked) {
					// Reset layers back to remembered state
					this.#enableRadioGroup(groupName, true);
					layerIDs.forEach((layerID) => {
						this.#toggleLayer(layerID, values["enabled"]);
					});
				} else {
					// Turn off layers
					this.#enableRadioGroup(groupName, false);
					layerIDs.forEach((layerID) => {
						this.#toggleLayer(layerID, false);
					});
				}

			} else {
				values["enabled"] = control.checked;
				layerIDs.forEach((layerID) => {
					this.#toggleLayer(layerID, control.checked);
				});
			}
		}

		this.renderTree();
		console.log("TREE WAS RE-RENDERED?");
	}



	/**
	 * Fires when a layer checkbox is selected.
	 * 
	 * @param checkbox - event source 
	 */
	onLayerChange(control) {
		let layerName = control.id;
		
		for(let [group, values] of Object.entries(this._treeData)) {
			let layerEntry = values[layerName];

			if(layerEntry != null) {

				// If this group uses radio buttons, disable all other layers
				let deselectOthers = this._radioHeaders.includes(group);
				if(deselectOthers) {
					this.#disableAllExcept(group, layerName);
				}

				// Toggle the selected layer
				let layerIDs = layerEntry["layers"];
				layerEntry["enabled"] = control.checked;

				layerIDs.forEach(layerID => {
					this.#toggleLayer(layerID, control.checked);
				});
				break;
			}
		}

		this.#updateGroupStates();
	}

	/**
	 * Hides all layers within the input group EXCEPT the input one.
	 * @param {String} group group/heading name. 
	 * @param {String} layerName layer name not to disable.
	 */
	#disableAllExcept(group, layerName) {
		let groupEntry = this._treeData[group];

		for(let [layer, values] of Object.entries(groupEntry)) {
			if(layer === layerName) continue;

			let layerIDs = values["layers"];
			values["enabled"] = false;

			layerIDs.forEach((layerID) => {
				this.#toggleLayer(layerID, false);
			});
		}	
	}

	/**
	 * Checks and updates heading selection state based on the state of 
	 * their sub-items.
	 * 
	 * @param {String} layerName	optional layer name to filter out groups where
	 * 								layer states have not changed.
	 */
	#updateGroupStates(layerName) {
		for(let [group, values1] of Object.entries(this._treeData)) {
			if(layerName != null && values1[layerName] == null) continue;

			let total = 0;
			let checked = 0;

			for(let [layer, values2] of Object.entries(values1)) {
				if(values2["enabled"]) {
					checked++;
				}
				total++;
			}
			
			let groupCheck = document.getElementById(group);
			if(groupCheck == null) {
				return;
			}

			if(this._radioHeaders.includes(group)) {
				groupCheck.checked = (checked > 0);
			} else {
				groupCheck.checked = (total == checked);
			}
		}
	}

	/**
	 * Finds all inputs with the name attribute matching the input group name
	 * and updates their enabled state.
	 * 
	 * @param {String} groupName group name.
	 * @param {boolean} enabled desired enabled state.
	 */
		 #enableRadioGroup(groupName, enabled) {
			let inputs = document.querySelectorAll("input[name='" + groupName + "']");
			inputs.forEach(input => {
				input.disabled = !enabled;
			});
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