/**
 * Example concrete instance of a DigitalTwinModule.
 */
export class CambridgeshireStationModule extends DigitalTwinModule {

	/**
	 * Initialise a new CambridgeshireStationModule.
	 */
	constructor() {
		super("Cambridgeshire Train Stations");
	}
	
	/**
	 * Load and add data sources to the MapBox map.
	 */
	 addSources() {
		this._map.addSource("station-data", {
			type: "geojson",
			data: "example/cambridgeshire_stations.geojson"
		});

		console.log("INFO: 'station-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map.
	 */
	addLayers() {
		this._map.addLayer({
			id: 'station-locations',
			source: 'station-data',
			type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': 5,
				'circle-color': "#FF0000",
				'circle-stroke-width': 1,
				'circle-stroke-color': "#000000",
			}
		});
		console.log("INFO: 'station-locations' layer has been added.");

		this._map.addLayer({
			id: 'station-labels',
			source: 'station-data',
			type: 'symbol',
			layout: {
				'visibility': 'visible',
				'text-field': ['concat',
					['get', 'Station Name'],
					' \n | ']
				,
				'text-font': ['DIN Offc Pro Medium', 'Arial Unicode MS Bold'],
				'text-size': 14,
				'text-offset': [0, -2],
			},
			paint: {
				"text-color": "#FF0000",
				"text-opacity": ['interpolate', ['exponential', 2], ['zoom'], 7, 0, 9, 1]
			}
		});
		console.log("INFO: 'station-labels' layer has been added.");	

		// Register as a single layer group
		this.registerLayerGroup("Train Stations", ["station-locations", "station-labels"], true);
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