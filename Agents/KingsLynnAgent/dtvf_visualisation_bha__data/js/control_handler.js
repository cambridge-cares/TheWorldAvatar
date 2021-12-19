/**
 * Handles building and interactions for the Camera, Terrain, and Layer Tree controls (i.e.
 * the ones on the left).
 * 
 * TODO: All tree functionality is handled manually within this file, would be good to clean it up
 * (or replace it with some external tree library) in future.
 */
class ControlHandler {

	// HTML for controls
	#controlHTML = `
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
			<div id="selectionsContainer"></div>
		</div>
	`;

	// MapBox map
	_map;

	//
	_datRegistry;
	
	// JSON metadata defining tree structure
	_treeSpecification;

	// Optional callback to fire when layer selections change
	_treeCallback;

	//
	_selectCallback;

	// HTML string of rendered tree.
	_treeHTML;

	/**
	 * Initialise a new MapControls instance.
	 * 
	 * @param {MapBox Map} map MapBox map 
	 * @param {DataRegistry} registry DataRegistry containing metadata
	 * @param {function} treeCallback Optional callback to fire when tree selections change
	 */
	initialise(map, registry, treeCallback = null) {
		this._map = map;
		this._registry = registry;
		this._treeCallback = treeCallback;
	}

	/**
	 * Generate the HTML for the control elements and add them to the 
	 * "controlsContainer" element.
	 * 
	 * @param {string} treeFile JSON file defining layer tree
	 */
	showControls(rootDirectories, selectedRootName, selectCallback) {
		this._selectCallback = selectCallback;

		document.getElementById("controlsContainer").innerHTML = this.#controlHTML;
		this.rebuildTree();

		let terrainContainer = document.getElementById("terrainContainer");
		let terrainSelect = terrainContainer.querySelector("input[id='" + DT.terrain + "']");
		if(terrainSelect != null) terrainSelect.checked = true;
		
		// Build dropdown to change root dir
		if(rootDirectories != null && Object.keys(rootDirectories).length > 1) {
			this.#buildRootDropdown(rootDirectories, selectedRootName);
		} 
		
		var selectionsContainer = document.getElementById("selectionsContainer");
		if(selectionsContainer != null) selectionsContainer.style.display = "block";

		// Build the initial dropdown selections
		let selectString = this.buildDropdown(this._registry.meta);
		document.getElementById("selectionsContainer").innerHTML += selectString;
	}

	/**
	 * 
	 * @param {*} rootDirectories 
	 */
	#buildRootDropdown(rootDirectories, selectedName) {
		var htmlString = `
			<div id="rootSelectContainer" style="margin-bottom: 10px;">
				<label for="root-dir-select">Data set:</label>
				<select id="root-dir-select" onchange="manager.onGroupSelectChange(this.id, this.value)">
		`;

		Object.keys(rootDirectories).forEach(function(key) {
			if(key === selectedName) {
				htmlString += `
					<option value="` + key + `" selected>` + key + `</option>
				`;
			} else {
				htmlString += `
					<option value="` + key + `">` + key + `</option>
				`;
			}
		});
		htmlString += `</div>`;

		var selectionsContainer = document.getElementById("selectionsContainer");
		if(selectionsContainer != null) {
			selectionsContainer.innerHTML += htmlString;
		}
	}

	/**
	 * 
	 * @param {*} currentMeta 
	 * @param {*} parentDivID 
	 * @returns 
	 */
	buildDropdown(currentMeta, parentDivID) {
		var htmlString = "";

		if(currentMeta["label"]) {
			let label = currentMeta["label"];
			let groups = currentMeta["groups"];

			htmlString += `
				<div id="selectContainer">
				<label for="` + label + `">` + label + `:</label>
				<select id="` + label + `" onchange="manager.onGroupSelectChange(this.id, this.value)">
				<option value="" disabled selected hidden>Select an option...</option>
			`;

			for(var i = 0; i < groups.length; i++) {
				let groupName = groups[i]["name"];
				let groupDir = groups[i]["directory"];
				let value = (parentDivID == null) ? groupDir : parentDivID + "/" + groupDir;
				htmlString += `
					<option value="` + value + `">` + groupName + `</option>
				`;
			}

			htmlString += `
				</select>
				<div id="select-` + label + `"></div>
				</div>
			`;
		}

		return htmlString;
	}

	/**
	 * 
	 * @param {*} groupID 
	 * @param {*} value 
	 */
	onGroupSelectChange(groupID, value) {
		let groupNames = value.split("/");
		let metaGroup = this._registry.getGroup(groupNames);

		if(metaGroup["groups"]) {
			// This group has subgroups, need to build more dropdowns
			let selectString = this.buildDropdown(metaGroup["groups"], value);
			document.getElementById("select-" + groupID).innerHTML = selectString;

		} else if(metaGroup["dataSets"]) {
			// Lowest level group, can show data now
			console.log("INFO: The following leaf group has been selected, '" + groupNames + "'.");
			if(this._selectCallback != null) {
				this._selectCallback(groupNames);
			}
		}
	}

	/**
	 * Rebuild the tree control.
	 */
	rebuildTree() {
		this.#renderTree();
		document.getElementById("layerContainer").innerHTML = this._treeHTML;

		// Update tree selection states
		for(var i = 0; i < this._treeSpecification.length; i++) {
			var treeEntry = this._treeSpecification[i];
			var totals = [0, 0];
			this.#countSelections(treeEntry, totals);

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
			if(inputBox == null) continue;

			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
			} else if(totals[1] == 0) {
				inputBox.checked = false;
			}
		}

	}

	/**
	 * Change the underlying MapBox style.
	 * 
	 * @param {String} mode {"light", "dark", "satellite", "satellite-streets"}
	 */
	 changeTerrain(mode) {
		if(mode === "light") {
			this._map.setStyle("mapbox://styles/mapbox/light-v10?optimize=true");
		} else if(mode === "dark") {
			this._map.setStyle("mapbox://styles/mapbox/dark-v10?optimize=true");
		} else if(mode === "satellite") {
			this._map.setStyle("mapbox://styles/mapbox/satellite-v9?optimize=true");
		} else if(mode === "satellite-streets") {
			this._map.setStyle("mapbox://styles/mapbox/satellite-streets-v11?optimize=true");
		} 

		// Store the current terrain as a global variable
		DT.terrain = mode;
	}

	/**
	 * Reset the camera to a default position.
	 * 
	 * @param {String} mode {"bird", "pitch"}
	 */
	changeCamera(mode) {
		if(mode === "bird") {
			this._map.flyTo({
				curve: 1.9,
				speed: 1.6,
				pitch: 0.0,
				bearing: 0.0,
				zoom: this._registry.globalMeta["defaultZoom"],
				center: this._registry.globalMeta["defaultCenter"]
			});
	
		} else if(mode === "pitch") {
			this._map.flyTo({
				curve: 1.9,
				speed: 1.6,
				pitch: 65,
				bearing: -30,
				zoom: this._registry.globalMeta["defaultZoom"],
				center: this._registry.globalMeta["defaultCenter"]
			});
		} 
	}
	
	/**
	 * Fires when a group checkbox within the layer control is selected.
	 * 
	 * @param {Element} control event source 
	 */
	 onLayerGroupChange(control) {
		var groupName = control.id;
		var newState = control.checked;

		for(var i = 0; i < this._treeSpecification.length; i++) {
			var treeEntry = this._treeSpecification[i];
			var result = [];

			this.#findGroup(groupName, treeEntry, result);
			if(result.length == 1) {
				this.#updateGroupSelection(null,result[0], newState);

				if(result[0]["controlType"] === "radio") {
					var layers = result[0]["layers"];

					for(var i = 0; i < layers.length; i++) {
						let inputBox = document.querySelector("input[id='" + layers[i]["layerName"] + "']");
						inputBox.disabled = !newState;
					}
				}
			}
		}
	}

	/**
	 * Fires when a layer control is selected.
	 * 
	 * @param {Element} checkbox event source 
	 */
	 onLayerChange(control) {
		let layerName = control.id;
		let newState = control.checked;

		for(var i = 0; i < this._treeSpecification.length; i++) {
			var treeEntry = this._treeSpecification[i];

			// Actually hide/show the layer
			this.#updateLayerSelection(null, treeEntry, layerName, newState);

			// Update tree selection states
			var totals = [0, 0];
			this.#countSelections(treeEntry, totals);

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
			if(inputBox == null) continue;

			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
			} else if(totals[1] == 0) {
				inputBox.checked = false;
			}
		}
	}

	/**
	 * After re-initialising the map, force the layer visibility to 
	 * match the existing selections in the layer tree.
	 */
	forceRefreshSelections() {
		var inputs = document.querySelectorAll("input.layerInput");

		for(var i = 0; i < inputs.length; i++) {
			var layerName = inputs[i].id;
			var layerEntry = [];
			for(var k = 0; k < this._treeSpecification.length; k++) {
				this.#findLayer(layerName, this._treeSpecification[k], layerEntry);
			}		

			if(layerEntry.length == 1) {
				for(var j = 0; j < layerEntry[0]["layerIDs"].length; j++) {
					this.#toggleLayer(layerEntry[0]["layerIDs"][j], inputs[i].checked);
				}
			}
		}
	}

	/**
	 * Reads the JSON metadata file that defines the tree structure.
	 * 
	 * @param {String} treeFile JSON file defining layer tree
	 */
	readTreeFile(treeFile) {
		var that = this;
		var promise = $.getJSON(treeFile, function(json) {
            that._treeSpecification = json;
        }).promise();

		return promise;
	}

	/**
	 * Show or hide a single (MapBox) layer on the map.
	 * 
	 * @param {String} layerID MapBox layer name.
	 * @param {boolean} visible desired visibility.
	 */
	#toggleLayer(layerID, visible) {
		try {
			this._map.setLayoutProperty(
				layerID,
				"visibility",
				(visible ? "visible" : "none")
			);

			// Is there a corresponding _clickable layer?
			if(this._map.getLayer(layerID + "_clickable") != null) {
				this._map.setLayoutProperty(
					layerID + "_clickable",
					"visibility",
					(visible ? "visible" : "none")
				);
			}

			// Is there a corresponding _cluster layer?
			if(this._map.getLayer(layerID + "_cluster") != null) {
				this._map.setLayoutProperty(
					layerID + "_cluster",
					"visibility",
					(visible ? "visible" : "none")
				);
			}
		} catch(err) {
			console.log("WARN: Could not toggle '" + layerID + "', it may have no initial 'visibility' layout property?");
		}
	}

	/**
	 * Builds the HTML required to show the Layer Tree.
	 */
	#renderTree() {
		this._treeHTML = `<p>Layers:</p>`;
		this._treeHTML += `<ul id="layerTree">`;
		
		for(var i = 0; i < this._treeSpecification.length; i++) {
			this.#renderIterate(this._treeSpecification[i]);
		}
		this._treeHTML += `</ul>`;
	}

	/**
	 * Recurses through elements within the _treeSpecification variable to build up the
	 * Layer Tree's HTML content.
	 * 
	 * @param {*} treeEntry current tree element.
	 */
	#renderIterate(treeEntry, currentGroup, controlType = "checkbox") {
		if(treeEntry["groupName"]) {
			var groupName = treeEntry["groupName"];

			this._treeHTML += `<li>`;
			this._treeHTML += "<input type='checkbox' onclick='manager.onLayerGroupChange(this);' id='" + groupName + "'>";
			this._treeHTML += "<label for='" + groupName + "'>" + groupName + "</label>";
			this._treeHTML += `<ul class="nested">`;

			controlType = (treeEntry["controlType"]) ? treeEntry["controlType"] : controlType;

			if(treeEntry["layers"]) {
				var layers = treeEntry["layers"];

				for(var i = 0; i < layers.length; i++) {
					this.#renderIterate(layers[i], groupName, controlType);
				}		
			}
			this._treeHTML += `</ul></li>`;


		} else if(treeEntry["layerName"]){
			var layerName = treeEntry["layerName"];

			if(!this.#anyLayersVisible(treeEntry["layerIDs"])) {
				// No layers for this entry have been added to the map
				return;
			}

			this._treeHTML += `<li>`
			this._treeHTML += "<input class='layerInput' type='" + controlType + "' onclick='manager.onLayerChange(this);' id='" + layerName + "' name='" + currentGroup + "'";

			if(treeEntry["defaultState"] === "visible") {
				treeEntry["currentState"] = "visible";
				this._treeHTML += ` checked>`;
			} else {
				treeEntry["currentState"] = "hidden";
				this._treeHTML += `>`;
			}
			this._treeHTML += "<label for='" + layerName + "'>" + layerName + "</label>";
			this._treeHTML += `</li>`		
		}
	}

	/**
	 * Returns true if any of the input layer ids are layers that existing on 
	 * the map (even if hidden).
	 * 
	 * @param {string[]} layerIDs mapbox layer ids
	 *  
	 * @returns true if any layers present
	 */
	#anyLayersVisible(layerIDs) {
		for(var i = 0; i < layerIDs.length; i++) {
			if(this._map.getLayer(layerIDs[i]) != null) {
				return true;
			}
		}
	
		return false;
	}

	/**
	 * Rescurses to find the tree element that represents the group with the input name.
	 * 
	 * @param {String} groupName target group name
	 * @param {JSONObject} treeEntry current tree entry
	 * @param {JSONObject[]} result array to hold result
	 */
	#findGroup(groupName, treeEntry, result) {
		if(treeEntry != null && treeEntry["groupName"]) {

			if(treeEntry["groupName"] === groupName) {
				result[0] = treeEntry;
				return;
			} else {
				let layers = treeEntry["layers"];
				for(var i = 0; i < layers.length; i++) {
					this.#findGroup(groupName, layers[i], result);
				}
			}
		}
	}

	/**
	 * Rescurses to find the tree element that represents the layer with the input name.
	 * 
	 * @param {String} layerName target layer name
	 * @param {JSONObject} treeEntry current tree entry
	 * @param {JSONObject[]} result array to hold result
	 */
	#findLayer(layerName, treeEntry, result) {
		if(treeEntry != null && treeEntry["layerName"]) {
			if(treeEntry["layerName"] === layerName) {
				result[0] = treeEntry;
			}
			
		} else if(treeEntry["groupName"]) {
			let layers = treeEntry["layers"];
			for(var i = 0; i < layers.length; i++) {
				this.#findLayer(layerName, layers[i], result);
			}
		}
	}

	/**
	 * Recurses to update the selection state of an entire group.
	 * 
	 * @param {JSONObject} parentEntry parent of current tree entry
	 * @param {JSONObject} treeEntry current tree entry
	 * @param {Boolean} newState desired selection state
	 */
	#updateGroupSelection(parentEntry, treeEntry, newState) {
		if(treeEntry["groupName"]) {
			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
			inputBox.checked = newState;

			let layers = treeEntry["layers"];
			for(var i = 0; i < layers.length; i++) {
				this.#updateGroupSelection(treeEntry, layers[i], newState)
			}

		} else {
			// If re-enabling a radio group, don't just switch all layers, use the default state
			if(newState && parentEntry["controlType"] === "radio") {
				newState = treeEntry["defaultState"] === "visible";
			} 

			this.#updateLayerSelection(parentEntry, treeEntry, treeEntry["layerName"], newState);
			let inputBox = document.querySelector("input[id='" + treeEntry["layerName"] + "']");
			inputBox.checked = newState;
		}
	}

	/**
	 * Recurses to update the selection state of a layer.
	 * 
	 * @param {JSONObject} parentEntry parent of current tree entry
	 * @param {JSONObject} treeEntry current tree entry
	 * @param {String} layerName target layer name
	 * @param {Boolean} newState desired selection state
	 */
	 #updateLayerSelection(parentEntry, treeEntry, layerName, newState) {
		if(treeEntry["layerName"] === layerName) {

			if(parentEntry != null && newState && parentEntry["controlType"] === "radio") {
				// Radio group, disable other layers if selecting a new one
				var layers = parentEntry["layers"];

				for(var i = 0; i < layers.length; i++) {
					if(layers[i]["layerName"] != layerName) {

						layers[i]["currentState"] = "hidden";

						if(this._treeCallback != null) {
							// Fire callback instead of default layer changing code
							this._treeCallback(layers[i]["layerName"], false);

						} else {
							// Get MapBox to actually change visibility
							for(var j = 0; j < layers[i]["layerIDs"].length; j++) {
								this.#toggleLayer(layers[i]["layerIDs"][j], false);
							}
						}
					}
				}
			}

			// Change the state of just this layer
			treeEntry["currentState"] = (newState) ? "visible" : "hidden";

			if(this._treeCallback != null) {
				// Fire callback instead of default layer changing code
				this._treeCallback(layerName, newState);
				
			} else {
				// Get MapBox to actually change visibility
				for(var j = 0; j < treeEntry["layerIDs"].length; j++) {
					this.#toggleLayer(treeEntry["layerIDs"][j], newState);
				}
			}

		} else if(treeEntry["layers"]) {
			// Iterate down into group
			var layers = treeEntry["layers"];
			for(var i = 0; i < layers.length; i++) {
				this.#updateLayerSelection(treeEntry, layers[i], layerName, newState);
			}
		}
	}

	/**
	 * Recurses to count the total and selected subentries within a tree node. Used to 
	 * determine if a group node should be selected or not.
	 * 
	 * @param {JSONObject} treeEntry current tree entry
	 * @param {Number[]} totals array for running counts [total, totalSelected]
	 */
	#countSelections(treeEntry, totals) {
		totals[0] += 1;

		if(treeEntry["layerName"]) {
			if(treeEntry["currentState"] === "visible") {
				totals[1] += 1		
			}
		} else if(treeEntry["groupName"]) {
			var layers = treeEntry["layers"];
			var tempTotals = [0, 0];
			
			for(var i = 0; i < layers.length; i++) {
				this.#countSelections(layers[i], tempTotals);
			}
			totals[0] += tempTotals[0];
			totals[1] += tempTotals[1];

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
			if(tempTotals[1] > 0 || (treeEntry["controlType"] === "radio" && tempTotals[1] > 0)) {
				inputBox.checked = true;
				totals[1] += 1;
			} else if(tempTotals[1] == 0) {
				inputBox.checked = false;
			}
		}
	}

}
// End of class.