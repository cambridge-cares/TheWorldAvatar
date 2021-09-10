/**
 * Example concrete instance of a DigitalTwinModule.
 */
export class CambridgeshireOrchardsModule extends DigitalTwinModule {

	/**
	 * Initialise a new CambridgeshireOrchardsModule.
	 */
	constructor() {
		super("Cambridgeshire Orchards");
	}
	
	/**
	 * Load and add data sources to the MapBox map.
	 */
	 addSources() {
		this._map.addSource("orchard-data", {
			type: "geojson",
			data: "example/cambridgeshire_orchards.geojson"
		});

		console.log("INFO: 'orchard-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map.
	 */
	addLayers() {
		this._map.addLayer({
			id: 'orchard-locations',
			source: 'orchard-data',
			type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': 5,
				'circle-color': "#0061FF",
				'circle-stroke-width': 1,
				'circle-stroke-color': "#000000",
			}
		});
		console.log("INFO: 'orchard-locations' layer has been added.");

		// Register as a single layer group
		this.registerLayerGroup("Orchards", ["orchard-locations"], true);
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