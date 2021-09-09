
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
		return this_.controlHTML;
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
}