/**
 * Handles building and interactions for the Layer Tree.
 * 
 * Note: All tree functionality is handled manually within this file, would be good to clean it up
 * (or replace it with some external tree library) in future.
 */
class DigitalTwinLayerTree {

	// MapBox map
	_map;

	// JSON metadata defining tree structure
	_treeSpecification;

	// Optional callback to fire when layer selections change
	_callback;

	// HTML string of rendered tree.
	_treeHTML;

	/**
	 * Initialise a new MapControls instance.
	 * 
	 * @param {MapBox Map} map MapBox map 
	 * @param {String} treeFile JSON file defining layer tree
	 * @param {function} callback Optional callback to fire when selections change
	 */
	constructor(map, treeFile, callback = null) {
		this._map = map;
		this._callback = callback;
		this.#readTreeFile(treeFile);
	}

	/**
	 * Reads the JSON metadata file that defines the tree structure.
	 * 
	 * @param {String} treeFile JSON file defining layer tree
	 */
	#readTreeFile(treeFile) {
		var that = this;

		var rawFile = new XMLHttpRequest();
		rawFile.onreadystatechange = function() {
			if (that._treeSpecification == null && rawFile.readyState == 4 && rawFile.status == "200") {
				console.log("Reading layer tree specification...");
				that._treeSpecification = JSON.parse(rawFile.responseText);
				that.#renderTree();
			} 
		}
		rawFile.open("GET", treeFile, true);
		rawFile.send();
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

		console.log("INFO: HTML for the layer tree has been constructed.");
		document.getElementById("layerContainer").innerHTML = this._treeHTML;

		// Update selection states
		for(var i = 0; i < this._treeSpecification.length; i++) {
			var treeEntry = this._treeSpecification[i];
			var totals = [0, 0];
			this.#countSelections(treeEntry, totals);

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
			} else if(totals[1] == 0) {
				inputBox.checked = false;
			}
		}
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
			this._treeHTML += "<input type='checkbox' onclick='manager.onGroupChange(this);' id='" + groupName + "'>";
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
	 * Fires when a group checkbox within the layer control is selected.
	 * 
	 * @param {Element} control event source 
	 */
	onGroupChange(control) {
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
			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
			} else if(totals[1] == 0) {
				inputBox.checked = false;
			}
		}
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
				console.log("Default state for '" + treeEntry["layerName"] + " is " + newState);
			} 

			console.log(newState);
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
						console.log("Layer '" + layers[i]["layerName"] + "' is now hidden");

						if(this._callback != null) {
							// Fire callback instead of default layer changing code
							this._callback(layers[i]["layerName"], false);

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
			console.log("Layer '" + layerName + "' is now " + treeEntry["currentState"]);

			if(this._callback != null) {
				// Fire callback instead of default layer changing code
				this._callback(layerName, newState);
				
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
	 * After re-initialising the map, force the layer visibility to 
	 * match the existing selections in the layer tree.
	 */
	forceRefreshSelections() {
		var inputs = document.querySelectorAll("input.layerInput");

		for(var i = 0; i < inputs.length; i++) {
			var layerName = inputs[i].id;
			console.log("State for " + layerName + " is " + inputs[i].checked);

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