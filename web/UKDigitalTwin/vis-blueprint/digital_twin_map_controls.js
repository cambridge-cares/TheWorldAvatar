
class MapControls {

	// HTML for controls
	_controlHTML = `
		<div id="controlContainer">
			<div id="cameraContainer">
				<p>Camera:</p>
				<a href="#" onclick="changeCamera('bird')">Bird's Eye</a>
				<br>
				<a href="#" onclick="changeCamera('pitch')">Pitched</a>
			</div>
			<div id="terrainContainer">
				<p>Terrain:</p>
				<input type="radio" name="terrain" id="light" onclick="changeTerrain('light')" checked>
				<label for="light">Light</label>
				<br>
				<input type="radio" name="terrain" id="satellite" onclick="changeTerrain('satellite')">
				<label for="satellite">Satellite</label>
				<br>
				<input type="radio" name="terrain" id="satellite-streets" onclick="changeTerrain('satellite-streets')">
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
			if(heading != "") {
				htmlString += "<li>";
				htmlString += "<input type='checkbox' onclick='globalController.onGroupChange(this);' id='" + heading + "'>";
				htmlString += "<label for='" + heading + "'>" + heading + "</label>";
				htmlString += "</li>";
			}
				
			if(heading == "") {
				htmlString += "<ul class='layerList listNoIndent'>";
			} else {
				htmlString += "<ul class='layerList'>";
			}
			
			for(let [name, values2] of Object.entries(values1)) {

				if(heading == "") {
					htmlString += "<li class='listNoIndent'>";
				} else {
					htmlString += "<li>";
				}


				let checked = this._treeData[heading][name]["enabled"];
				if(checked) {
					htmlString += "<input type='checkbox' onclick='globalController.onLayerChange(this);' id='" + name + "' checked>";
				} else {
					htmlString += "<input type='checkbox' onclick='globalController.onLayerChange(this);' id='" + name + "'>";
				}
			
				htmlString += "<label for='" + name + "'>" + name + "</label>";
				htmlString += "</li>";
			}
			htmlString += "</ul>"
		}
		htmlString += "</ul>";

		// Add to the document
		document.getElementById("layerContainer").innerHTML = htmlString;
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
			values["enabled"] = control.checked;

			layerIDs.forEach((layerID) => {
				this.#toggleLayer(layerID, control.checked);
			});
		}
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
				let layerIDs = layerEntry["layers"];
				layerEntry["enabled"] = control.checked;

				layerIDs.forEach(layerID => {
					this.#toggleLayer(layerID, control.checked);
				});
			}
		}

		console.log("onLayerChange()");
		this.#flarb();
	}

	#flarb(layerName) {
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
			console.log(groupCheck);

			if(total == checked) {
				groupCheck.checked = true;
				console.log(group + " should now be true!");
			} else {
				groupCheck.checked = false;
				console.log(group + " should now be false!");
			}
		}
	}
}