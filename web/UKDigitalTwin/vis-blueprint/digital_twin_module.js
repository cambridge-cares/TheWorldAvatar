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

	// Dictionary for layers as grouped in controls
	_layerGroups = {};

	// Are multiple layers defined in this module allowed to be shown at once?
	_viewMultiple = true;

	/**
	 * Initialise a new DigitalTwinModule instance.
	 * 
	 * @param {String} name module name.
	 * @param {boolean} viewMultiple are multiple layers defined in this module allowed to be shown at once?
	 */
	constructor(name, viewMultiple) {
		this._name = name;
		this._viewMultiple = viewMultiple;
	}

	/**
	 * Returns the name of the module.
	 */
	get name() {
		return this._name;
	}

	/**
	 * Returns the view multiple state of the module.
	 */
	get viewMultiple() {
		return this._viewMultiple;
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
	 * Register a group of MapBox layers, enabling a single show/hide control.
	 * 
	 * @param {String} name user facing name for group. 
	 * @param {String[]} layerNames array of MapBox layer names.
	 * @param {boolean} should this group be enabled by default?
	 */
	registerLayerGroup(name, layerNames, enabled) {
		let layerGroup = this._layerGroups[name];

		if(layerGroup == null) {
			this._layerGroups[name] = {
				"layers": layerNames,
				"visible": enabled
			}
		}
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
	 * Generate and return HTML content for display in the legend.
	 */
	getLegendContent() {
		throw new Error("The 'getLegendContent()' method must be implemented in a concrete subclass!");
	}

	/**
	 * If registered for selections, this triggers when an individual
	 * item on the map is selected. 
	 * 
	 * @param {String} layerName the name of the layer containing the selected item.
	 * @param {} coordinates coordinates of the selected item.
	 * @param {*} features GeoJSON features of the selected item.
	 */
	onSelection(layerName, coordinates, features) {
		throw new Error("The 'onSelection()' method must be implemented in a concrete subclass!");
	}

}