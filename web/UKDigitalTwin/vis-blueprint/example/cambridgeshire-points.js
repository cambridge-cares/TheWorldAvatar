/**
 * Example concrete instance of a DigitalTwinModule.
 */
export class CambridgeshirePointsModule extends DigitalTwinModule {

	/**
	 * Initialise a new CambridgeshirePointsModule.
	 */
	constructor() {
		super("Cambridgeshire Points");
	}
	
	/**
	 * Load and add data sources to the MapBox map.
	 */
	 addSources() {
		this._map.addSource("traffic-counters-data", {
			type: "geojson",
			data: "example/traffic-counters.geojson"
		});
		console.log("INFO: 'traffic-counters-data' source has been added.");

		this._map.addSource("rail-stations-data", {
			type: "geojson",
			data: "example/rail-stations.geojson"
		});
		console.log("INFO: 'rail-stations-data' source has been added.");

		this._map.addSource("orchards-data", {
			type: "geojson",
			data: "example/orchards.geojson"
		});
		console.log("INFO: 'orchards-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map.
	 */
	addLayers() {
		this._map.addLayer({
			id: 'traffic-counters-layer',
			source: 'traffic-counters-data',
			type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': 5,
				'circle-color': "#FFCDD2",
				'circle-stroke-width': 1,
				'circle-stroke-color': "#000000",
			}
		});
		console.log("INFO: 'traffic-counters-layer' layer has been added.");

		this._map.addLayer({
			id: 'rail-stations-layer',
			source: 'rail-stations-data',
			type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': 5,
				'circle-color': "#F44336",
				'circle-stroke-width': 1,
				'circle-stroke-color': "#000000",
			}
		});
		console.log("INFO: 'rail-stations-layer' layer has been added.");

		this._map.addLayer({
			id: 'orchards-layer',
			source: 'orchards-data',
			type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': 5,
				'circle-color': "#B71C1C",
				'circle-stroke-width': 1,
				'circle-stroke-color': "#000000",
			}
		});
		console.log("INFO: 'orchards-layer' layer has been added.");

		// Register each MapBox layer separately under a single heading
		this.registerLayerGroup("Rail Stations", ["rail-stations-layer"], true, "Locations");
		this.registerLayerGroup("Automatic Traffic Counters", ["traffic-counters-layer"], true, "Locations");
		this.registerLayerGroup("Community Orchards", ["orchards-layer"], true, "Locations");
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