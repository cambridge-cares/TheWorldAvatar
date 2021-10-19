/**
 * This class acts as a module that allows a central visualisation script to load,
 * display, and allow interactions with a single data domain.
 * 
 * @author Michael Hillman
 */
class DigitalTwinModule {

	// Name of this module
	_name;
	
	// MapBox instance
	_map;

	/**
	 * Initialise a new DigitalTwinModule instance.
	 * 
	 * @param {String} name module name.
	 */
	constructor(name) {
		this._name = name;
	}

	/**
	 * Returns the name of the module.
	 */
	get name() {
		return this._name;
	}

	/**
	 * Sets the MapBox map instance.
	 * 
	 * @param {MapBox map} map 
	 */
	setMap(map) {
		this._map = map;
	}

	/**
	 * Load and add data sources to the MapBox map.
	 */
	addSources() {
		throw new Error("The 'addSources()' method must be implemented in a concrete subclass!");
	}

	/**
	 * Generate and add layers to the MapBox map.
	 */
	addLayers() {
		throw new Error("The 'addSources()' method must be implemented in a concrete subclass!");
	}

	/**
	 * Will attempt to automatically detect and load GeoJSON and TimeSeries JSON files. Note that this will ONLY work
	 * if the data strictly follows the format outlined below.
	 * 
	 * Format:
	 *     - Each GeoJSON file will be loaded as MapBox data source using the file name (without suffix) as the source name.
	 *     - If a GeoJSON has matching TimeSeries JSON files then these will be loaded:
	 *         - For a TimeSeries file to match a GeoJSON, its name must match the format "NAME-SET-timeseries.json", where
	 *           "NAME" matches the name of the GeoJSON file, and "SET" is an optional data set name (i.e. "daily", or "weekly").
	 *           Note that the NAME and SET must not contain hyphon/dash characters.
	 * 
	 * @param {String} directory location of directory containing data.
	 */
	autoDetectData(directory) {

	}

	/**
	 * 
	 * @param {*} layerName 
	 * @param {*} mouseMove 
	 * @param {*} mouseEnter 
	 * @param {*} mouseExit 
	 * @param {*} mouseClick 
	 */
	addMouseEffects(layerName, mouseMove, mouseEnter, mouseExit, mouseClick) {
		if(mouseMove) {
			this._map.on("mousemove", layerName, (e) => {
				if(e.features != null && e.features.length > 0) {
					// Pass to logic in concrete module
					this.onMouseMove(layerName, e.features[0]);
				}
			});
		}

		if(mouseEnter) {
			this._map.on("mouseenter", layerName, (e) => {
				if(e.features != null && e.features.length > 0) {
					// Pass to logic in concrete module
					this.onMouseEnter(DT.popup, layerName, e.features[0], e);
				}
			});
		}

		if(mouseExit) {
			this._map.on("mouseleave", layerName, () => {
				// Pass to logic in concrete module
				this.onMouseExit(DT.popup, layerName);
			});
		}

		if(mouseClick) {
			this._map.on("click", layerName, (e) => {
				// Pass to logic in concrete module
				this.onMouseClick(layerName, e.features[0], e);
			});
		}
	}

	/**
	 * @param {*} layerName 
	 * @param {*} feature 
	 */
	onMouseMove(layerName, feature) {
		// Needs to be implemented in your concrete module class.
	}

	/**
	 * 
	 * @param {*} popup 
	 * @param {*} layerName 
	 * @param {*} feature 
	 */
	onMouseEnter(popup, layerName, feature, event) {
		// Needs to be implemented in your concrete module class.
	}

	/**
	 * 
	 * @param {*} popup 
	 * @param {*} layerName 
	 */
	onMouseExit(popup, layerName) {
		// Needs to be implemented in your concrete module class.
	}

	/**
	 * 
	 * @param {*} layerName 
	 * @param {*} feature 
	 * @param {*} event 
	 */
	onMouseClick(layerName, feature, event) {
		// Needs to be implemented in your concrete module class.
	}

	/**
	 * Generate and return HTML content for display in the legend.
	 */
	 getLegendContent() {
		throw new Error("The 'getLegendContent()' method must be implemented in a concrete subclass!");
	}

}
// End of class