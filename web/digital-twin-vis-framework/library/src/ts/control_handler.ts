/**
 * Handles building and interactions for the Camera, Terrain, and Layer Tree controls (i.e.
 * the ones on the left).
 * 
 * TODO: All tree functionality is handled manually within this file, would be good to clean it up
 * (or replace it with some external tree library) in future.
 */
class ControlHandler {

    private treeFile: string;

	// JSON metadata defining tree structure
    public static TREE_STATE;

	// Optional callback to fire when layer selections change
	private treeCallback;

	//
	private selectionCallback;

	// HTML string of rendered tree.
	private treeHTML;

    /**
     * 
     */
    constructor() {
        //this.treeCallback = treeCallback;
        window.terrain = "light";
    }

    /**
     * 
     */
    private setupCollapses() {
        let container = document.getElementById("controlContainer");
        let titles = container.querySelectorAll(".controlTitle p");

        titles.forEach(title => {
            title.addEventListener("click", function() {
                let block = title.parentElement.parentElement;
                if(block.classList.contains("collapsed")) {
                    block.classList.remove("collapsed");
                } else {
                    block.classList.add("collapsed");
                }
            });
        });
    }
	/**
	 * Generate the HTML for the control elements and add them to the 
	 * "controlsContainer" element.
	 * 
	 * @param {string} treeFile JSON file defining layer tree
	 */
	showControls() {
		//this.selectionCallback = selectionCallback;

        // Load controls.html file
        loadHTML("./html/controls.html").then(text => {
            document.getElementById("controlsContainer").innerHTML = text;
            this.setupCollapses();

            // Rebuild the layer tree
            this.rebuildTree();

            // Ensure selected terrain option is right
            let terrainContainer = document.getElementById("terrainContainer");
            let terrainSelect = terrainContainer.querySelector("input[id='" + window.terrain + "']") as HTMLInputElement;
            if(terrainSelect != null) terrainSelect.checked = true;
        });
	}

	/**
	 * Shows debugging info, like mouse position.
	 */
	showDeveloperControls() {
		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "none !important";

		let self = this;
		MapHandler.MAP.on("mousemove", function(event) {
			self.updateDeveloperControls(event);
		});
	}

	/**
	 * Update developer info panel.
	 */
	private updateDeveloperControls(event) {
		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "block";

		let lng = event.lngLat.lng.toFixed(5);
		let lat = event.lngLat.lat.toFixed(5);
		developerInfo.innerHTML = `
			<table width="100%">
				<tr>
					<td width="35%">Longitude:</td>
					<td width="65%">` + lng + `</td>
				</tr>
				<tr>
					<td width="35%">Latitude:</td>
					<td width="65%">` + lat + `</td>
				</tr>
			</table>
		`;
	}


	// /**
	//  * Builds a drop-down control to allow users to change between the
	//  * registered root data directoties.
	//  * 
	//  * @param {{String, String}} rootDirectories map of name to directory location.
	//  */
	// #buildRootDropdown(rootDirectories, selectedName) {
	// 	var htmlString = `
	// 		<div id="rootSelectContainer" style="margin-bottom: 10px;">
	// 			<label for="root-dir-select">Data set:</label>
	// 			<select id="root-dir-select" onchange="manager.onGroupSelectChange(this.id, this.value)">
	// 	`;

	// 	Object.keys(rootDirectories).forEach(function(key) {
	// 		if(key === selectedName) {
	// 			htmlString += `
	// 				<option value="` + key + `" selected>` + key + `</option>
	// 			`;
	// 		} else {
	// 			htmlString += `
	// 				<option value="` + key + `">` + key + `</option>
	// 			`;
	// 		}
	// 	});
	// 	htmlString += `</div>`;

	// 	var selectionsContainer = document.getElementById("selectionsContainer");
	// 	if(selectionsContainer != null) {
	// 		selectionsContainer.innerHTML += htmlString;
	// 	}
	// }

	// /**
	//  * Builds a drop-down control to allow the user to change the data group
	//  * represented by the input meta object.
	//  * 
	//  * @param {JSONObject} currentMeta meta object containing data groups
	//  * @param {String} parentDivID id of parent div
	//  * 
	//  * @returns HTML string for drop-down
	//  */
	// buildDropdown(currentMeta, parentDivID) {
	// 	var htmlString = "";

	// 	if(currentMeta["label"]) {
	// 		let label = currentMeta["label"];
	// 		let groups = currentMeta["groups"];

	// 		htmlString += `
	// 			<div id="selectContainer">
	// 			<label for="` + label + `">` + label + `:</label>
	// 			<select id="` + label + `" onchange="manager.onGroupSelectChange(this.id, this.value)">
	// 		`;

	// 		for(var i = 0; i < groups.length; i++) {
	// 			let groupName = groups[i]["name"];
	// 			let groupDir = groups[i]["directory"];
	// 			let value = (parentDivID == null) ? groupDir : parentDivID + "/" + groupDir;

	// 			if(i == 0) {
	// 				htmlString += `<option value="` + value + `" selected>` + groupName + `</option>`;
	// 			} else {
	// 				htmlString += `<option value="` + value + `">` + groupName + `</option>`;
	// 			}
	// 		}

	// 		htmlString += `
	// 			</select>
	// 			<div id="select-` + label + `"></div>
	// 			</div>
	// 		`;
	// 	}
	// 	return htmlString;
	// }

	// /**
	//  * Fires when a the data group selection changes.
	//  * 
	//  * @param {String} groupID id of group
	//  * @param {String} value full id of group
	//  */
	// onGroupSelectChange(groupID, value) {
	// 	let groupNames = value.split("/");
	// 	let metaGroup = this.registry.getGroup(groupNames);

	// 	if(metaGroup["groups"]) {
	// 		// This group has subgroups, need to build more dropdowns
	// 		let selectString = this.buildDropdown(metaGroup["groups"], value);
	// 		document.getElementById("select-" + groupID).innerHTML = selectString;

	// 		var newSelect = document.getElementById("select-" + groupID).querySelector("select");
	// 		newSelect.dispatchEvent(new Event('change', {bubbles: true}));

	// 	} else if(metaGroup["dataSets"]) {
	// 		// Lowest level group, can show data now
	// 		console.log("INFO: The following leaf group has been selected, '" + groupNames + "'.");
	// 		if(this.selectCallback != null) {
	// 			this.selectCallback(groupNames);
	// 		}
	// 	}
	// }

	/**
	 * Rebuild the tree control.
	 */
	rebuildTree() {
		this.renderTree();
		document.getElementById("layerTreeContainer").innerHTML = this.treeHTML;

		// Update tree selection states
		for(var i = 0; i < ControlHandler.TREE_STATE.length; i++) {
			var treeEntry = ControlHandler.TREE_STATE[i];
			var totals = [0, 0];
			this.countSelections(treeEntry, totals);

            console.log("TOTALS FOR " + treeEntry["groupName"] + " ARE " + totals);

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']") as HTMLInputElement;
			if(inputBox == null) continue;

			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
			} else if(totals[1] == 0) {
				inputBox.checked = false;
			}
		}
	}
	
	// /**
	//  * Fires when a group checkbox within the layer control is selected.
	//  * 
	//  * @param {Element} control event source 
	//  */
	//  onLayerGroupChange(control) {
	// 	var groupName = control.id;
	// 	var newState = control.checked;

	// 	for(var i = 0; i < ControlHandler.TREE_STATE.length; i++) {
	// 		var treeEntry = ControlHandler.TREE_STATE[i];
	// 		var result = [];

	// 		this.#findGroup(groupName, treeEntry, result);
	// 		if(result.length == 1) {
	// 			this.#updateGroupSelection(null,result[0], newState);

	// 			if(result[0]["controlType"] === "radio") {
	// 				var layers = result[0]["layers"];

	// 				for(var i = 0; i < layers.length; i++) {
	// 					let inputBox = document.querySelector("input[id='" + layers[i]["layerName"] + "']");
	// 					inputBox.disabled = !newState;
	// 				}
	// 			}
	// 		}
	// 	}
	// }

	// /**
	//  * After re-initialising the map, force the layer visibility to 
	//  * match the existing selections in the layer tree.
	//  */
	// forceRefreshSelections() {
	// 	var inputs = document.querySelectorAll("input.layerInput");

	// 	for(var i = 0; i < inputs.length; i++) {
	// 		var layerName = inputs[i].id;
	// 		var layerEntry = [];
	// 		for(var k = 0; k < ControlHandler.TREE_STATE.length; k++) {
	// 			this.#findLayer(layerName, ControlHandler.TREE_STATE[k], layerEntry);
	// 		}		

	// 		if(layerEntry.length == 1) {
	// 			for(var j = 0; j < layerEntry[0]["layerIDs"].length; j++) {
	// 				this.#toggleLayer(layerEntry[0]["layerIDs"][j], inputs[i].checked);
	// 			}
	// 		}
	// 	}
	// }

	/**
	 * Reads the JSON metadata file that defines the tree structure.
	 * 
	 * @param treeFile JSON file defining layer tree
	 */
	public async readTreeFile(group: DataGroup) {
        let rootGroup = DataUtils.getRootGroup(group);
        let newTreeFile = rootGroup.location + "/tree.json";
        if(newTreeFile === this.treeFile) return;

		return await $.getJSON(newTreeFile, function(json) {
            ControlHandler.TREE_STATE = json;
        });
	}



	/**
	 * Builds the HTML required to show the Layer Tree.
	 */
	private renderTree() {
        //DT.treeDictionary = {};
		this.treeHTML = `<ul id="layerTree">`;
		
		for(var i = 0; i < ControlHandler.TREE_STATE.length; i++) {
			this.renderIterate(ControlHandler.TREE_STATE[i], null);
		}
		this.treeHTML += `</ul>`;
	}

	/**
	 * Recurses through elements within the _TREE_STATE variable to build up the
	 * Layer Tree's HTML content.
	 * 
	 * @param {*} treeEntry current tree element.
	 */
	private renderIterate(treeEntry, currentGroup, controlType = "checkbox") {
		if(treeEntry["groupName"]) {
			var groupName = treeEntry["groupName"];

			this.treeHTML += `<li>`;
			this.treeHTML += "<input type='checkbox' onclick='manager.onLayerGroupChange(this);' id='" + groupName + "'>";
			this.treeHTML += "<label for='" + groupName + "'>" + groupName + "</label>";
			this.treeHTML += `<ul class="nested">`;

			controlType = (treeEntry["controlType"]) ? treeEntry["controlType"] : controlType;

			if(treeEntry["layers"]) {
				var layers = treeEntry["layers"];

				for(var i = 0; i < layers.length; i++) {
					this.renderIterate(layers[i], groupName, controlType);
				}		
			}
			this.treeHTML += `</ul></li>`;


		} else if(treeEntry["layerName"]){
			if(!MapBoxUtils.anyLayersAdded(treeEntry["layerIDs"])) {
				// No layers for this entry have been added to the map
				return;
			}

			// HTML start
			var layerName = treeEntry["layerName"];
			this.treeHTML += `<li>`
			this.treeHTML += "<input class='layerInput' type='" + controlType + "' onclick='manager.onLayerChange(this);' id='" + layerName + "' name='" + currentGroup + "'";

            // Set default visibility state
            let visible = true;

            if(!treeEntry["visible"]) {
                visible = MapBoxUtils.anyLayersVisible(treeEntry["layerIDs"]);
                treeEntry["visible"] = visible;
            } else {
                visible = treeEntry["visible"];
            }
            this.treeHTML += (visible) ? ` checked>` : `>`;

			// HTML end
			this.treeHTML += "<label for='" + layerName + "'>" + layerName + "</label>";
			this.treeHTML += `</li>`		
		}
	}



	// /**
	//  * Rescurses to find the tree element that represents the group with the input name.
	//  * 
	//  * @param {String} groupName target group name
	//  * @param {JSONObject} treeEntry current tree entry
	//  * @param {JSONObject[]} result array to hold result
	//  */
	// #findGroup(groupName, treeEntry, result) {
	// 	if(treeEntry != null && treeEntry["groupName"]) {

	// 		if(treeEntry["groupName"] === groupName) {
	// 			result[0] = treeEntry;
	// 			return;
	// 		} else {
	// 			let layers = treeEntry["layers"];
	// 			for(var i = 0; i < layers.length; i++) {
	// 				this.#findGroup(groupName, layers[i], result);
	// 			}
	// 		}
	// 	}
	// }

	// /**
	//  * Rescurses to find the tree element that represents the layer with the input name.
	//  * 
	//  * @param {String} layerName target layer name
	//  * @param {JSONObject} treeEntry current tree entry
	//  * @param {JSONObject[]} result array to hold result
	//  */
	// #findLayer(layerName, treeEntry, result) {
	// 	if(treeEntry != null && treeEntry["layerName"]) {
	// 		if(treeEntry["layerName"] === layerName) {
	// 			result[0] = treeEntry;
	// 		}
			
	// 	} else if(treeEntry["groupName"]) {
	// 		let layers = treeEntry["layers"];
	// 		for(var i = 0; i < layers.length; i++) {
	// 			this.#findLayer(layerName, layers[i], result);
	// 		}
	// 	}
	// }

	// /**
	//  * Recurses to update the selection state of an entire group.
	//  * 
	//  * @param {JSONObject} parentEntry parent of current tree entry
	//  * @param {JSONObject} treeEntry current tree entry
	//  * @param {Boolean} newState desired selection state
	//  */
	// #updateGroupSelection(parentEntry, treeEntry, newState) {
	// 	if(treeEntry["groupName"]) {
	// 		let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']");
	// 		inputBox.checked = newState;

	// 		let layers = treeEntry["layers"];
	// 		for(var i = 0; i < layers.length; i++) {
	// 			this.#updateGroupSelection(treeEntry, layers[i], newState)
	// 		}

	// 	} else {
	// 		// If re-enabling a radio group, don't just switch all layers, use the default state
	// 		if(newState && parentEntry["controlType"] === "radio") {
	// 			newState = treeEntry["defaultState"] === "visible";
	// 		} 

	// 		this.#updateLayerSelection(parentEntry, treeEntry, treeEntry["layerName"], newState);
	// 		let inputBox = document.querySelector("input[id='" + treeEntry["layerName"] + "']");
	// 		inputBox.checked = newState;
	// 	}
	// }

    	/**
	 * Fires when a layer control is selected.
	 * 
	 * @param {Element} checkbox event source 
	 */
	onLayerChange(control) {
		let layerName = control.id;
		let newState = control.checked;

		for(var i = 0; i < ControlHandler.TREE_STATE.length; i++) {
			var treeEntry = ControlHandler.TREE_STATE[i];

			// Actually hide/show the layer
			this.updateLayerSelection(null, treeEntry, layerName, newState);

			// Update tree selection states
			var totals = [0, 0];
			this.countSelections(treeEntry, totals);

            console.log(treeEntry["groupName"] + " is now " + totals);

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']") as HTMLInputElement;
			if(inputBox == null) continue;

			if((totals[0] == totals[1]) || (treeEntry["controlType"] === "radio" && totals[1] > 0)) {
				inputBox.checked = true;
               // treeEntry["visible"] = true;
                console.log("1");
			} else if(totals[1] == 0) {
				inputBox.checked = false;
                //treeEntry["visible"] = false;
                console.log("2");
			} else {
                console.log("3");
            }
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
	private updateLayerSelection(parentEntry, treeEntry, layerName, newState) {
		if(treeEntry["layerName"] === layerName) {

			if(parentEntry != null && newState && parentEntry["controlType"] === "radio") {
				// Radio group, disable other layers if selecting a new one
				var layers = parentEntry["layers"];

				for(var i = 0; i < layers.length; i++) {
					if(layers[i]["layerName"] != layerName) {

						layers[i]["visible"] = false;
                        for(var j = 0; j < layers[i]["layerIDs"].length; j++) {
                            MapBoxUtils.toggleLayer(layers[i]["layerIDs"][j], false);
                        }
					}
				}
			}

			// Change the state of just this layer
			treeEntry["visible"] = (newState) ? true : false;
            for(var j = 0; j < treeEntry["layerIDs"].length; j++) {
                MapBoxUtils.toggleLayer(treeEntry["layerIDs"][j], newState);
            }

		} else if(treeEntry["layers"]) {
			// Iterate down into group
			var layers = treeEntry["layers"];
			for(var i = 0; i < layers.length; i++) {
				this.updateLayerSelection(treeEntry, layers[i], layerName, newState);
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
	private countSelections(treeEntry, totals) {
		totals[0] += 1;

		if(treeEntry["layerName"]) {
			if(treeEntry["visible"] === true) {
                totals[1] += 1;
            } else {
                console.log(treeEntry);
            }

		} else if(treeEntry["groupName"]) {
			var layers = treeEntry["layers"];
			var tempTotals = [0, 0];
			
			for(var i = 0; i < layers.length; i++) {
				this.countSelections(layers[i], tempTotals);
			}
			totals[0] += tempTotals[0];
			totals[1] += tempTotals[1];

			let inputBox = document.querySelector("input[id='" + treeEntry["groupName"] + "']") as HTMLInputElement;
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