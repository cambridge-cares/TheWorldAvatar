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