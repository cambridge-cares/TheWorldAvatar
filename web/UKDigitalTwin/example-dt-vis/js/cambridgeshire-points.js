/**
 * Example concrete instance of a DigitalTwinModule.
 * 
 * This module handles loading data sources, creating layers, and handling
 * event logic for a series of example point locations with Cambridgeshire.
 */
export class CambridgeshirePointsModule extends DigitalTwinModule {

	/**
	 * Initialise a new CambridgeshirePointsModule.
	 */
	constructor() {
		super("Cambridgeshire Locations");
	}
	
	/**
	 * Load and add data sources to the MapBox map.
	 */
	 addSources() {
		this._map.addSource("traffic-counters-data", {
			type: "geojson",
			data: "./data/traffic-counters.geojson"
		});
		console.log("INFO: 'traffic-counters-data' source has been added.");

		this._map.addSource("rail-stations-data", {
			type: "geojson",
			data: "./data/rail-stations.geojson"
		});
		console.log("INFO: 'rail-stations-data' source has been added.");

		this._map.addSource("orchards-data", {
			type: "geojson",
			data: "./data/orchards.geojson"
		});
		console.log("INFO: 'orchards-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map. Mouse effects can 
	 * also be turned on here by calling the addMouseEffects() method.
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
		this.addMouseEffects("traffic-counters-layer", false, true, true, true);
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
		this.addMouseEffects("rail-stations-layer", false, true, true, true);
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
		this.addMouseEffects("orchards-layer", false, true, true, true);
		console.log("INFO: 'orchards-layer' layer has been added.");
	}

	/**
	 * Fires when a mouse enters a plotted location. The logic that runs here
	 * will likely be specific to your data sources, so you will need to 
	 * determine what data to show/logic to run here yourself.
	 * 
	 * @param {?} popup popup element
	 * @param {String} layerName name of layer containing location
	 * @param {JSONObject} feature triggered feature
	 * @param {MouseEvent} event mouse event
	 */
	onMouseEnter(popup, layerName, feature, event) {
		this._map.getCanvas().style.cursor = 'pointer';

		// Get correct co-ords
		var coordinates = feature.geometry.coordinates.slice();
		while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
			coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
		}

		// Get appropriate description for layer
		var description = null;
		switch(layerName) {
			case "traffic-counters-layer":
				description = "Road: " + feature.properties["Road Name"];
			break;
			case "rail-stations-layer":
				description = "Station: " + feature.properties["Station Name"];
			break;
			case "orchards-layer":
				description = "Orchard: " + feature.properties["Village"];
			break;
		}

		// Show popup
		var html = "<b>" + description + "</b></br>";
		html += "<em>" + coordinates[1].toFixed(5) + ", " + coordinates[0].toFixed(5) + "</em>"
		popup.setLngLat(coordinates).setHTML(html).addTo(this._map);
	}

	/**
	 * Fires when a mouse leaves a plotted location. This example just hides
	 * the popup, but data/layer specific actions could be taken here.
	 * 
	 * @param {?} popup popup element
	 * @param {String} layerName name of layer containing location
	 */
	onMouseExit(popup, layerName) {
		this._map.getCanvas().style.cursor = '';
		popup.remove();
	}

	/**
	 * Fires when the user selects a plotted location. This example picks
	 * some properties from the data source and sends them to the Side Panel
	 * for display, but data/layer specific actions could be taken here.
	 * 
	 * @param {String} layerName name of layer containing location
	 * @param {JSONObject} feature location selected
	 * @param {MouseEvent} event mouse event
	 */
	 onMouseClick(layerName, feature, event) {
		// Needs to be implemented in your concrete module class.
		var sidePanel = DT.sidePanelHandler;
		
		// Clear existing side panel content
		sidePanel.setContent("");

		// Show the legend (in case it had been hidden before)
		sidePanel.toggleLegend(true);
		
		switch(layerName) {
			case "traffic-counters-layer":
				sidePanel.setTitle(feature.properties["Road Name"]);
			break;
			case "rail-stations-layer":
				sidePanel.setTitle(feature.properties["Station Name"]);
			break;
			case "orchards-layer":
				sidePanel.setTitle(feature.properties["Village"]);

				// If we're selecting an Orchard, we can also get it's 
				// time series data via the DT.timeSeriesHandler
				var orchardID = feature.id;

				// Show Time Series data for this entry
				DT.timeSeriesHandler.showData(layerName, "daily-data", orchardID);

			break;
		}

		// Show the metadata
		sidePanel.setProperties(feature.properties);
	}

	/**
	 * Generate and return HTML content for display in the legend.
	 */
	getLegendContent() {
		var legendContent = `
			<img src="./img/legend-points.svg" width="100" height="100" />
		`;
		return legendContent;
	}

}
// End of class.