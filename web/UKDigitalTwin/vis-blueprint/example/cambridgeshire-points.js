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
	 * Fires when a mouse enters a plotted location.
	 * 
	 * @param {*} popup popup element
	 * @param {*} layerName name of layer containing location
	 * @param {*} feature triggered feature
	 * @param {*} event mouse event
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
	 * Fires when a mouse leaves a plotted location.
	 * 
	 * @param {*} popup popup element
	 * @param {*} layerName name of layer containing location
	 */
	onMouseExit(popup, layerName) {
		this._map.getCanvas().style.cursor = '';
		popup.remove();
	}

	/**
	 * 
	 * @param {*} layerName 
	 * @param {*} feature 
	 * @param {*} event 
	 */
	 onMouseClick(layerName, feature, event) {
		// Needs to be implemented in your concrete module class.
		var sidePanel = DT.sideHandler;
		
		switch(layerName) {
			case "traffic-counters-layer":
				sidePanel.setTitle(feature.properties["Road Name"]);
			break;
			case "rail-stations-layer":
				sidePanel.setTitle(feature.properties["Station Name"]);
			break;
			case "orchards-layer":
				sidePanel.setTitle(feature.properties["Village"]);
			break;
		}

		// Set some content
		sidePanel.setContent(`
			<div style="border: 1px solid grey; height: 100%;">
				In this area, general HTML content can be added. This could be a paragraph
				explaining the location the user has selected, data loaded from an external
				source, an image, or even an element generated from another JavaScript file 
				(like a chart or a diagram).
			</div>
		`);

		// Show the metadata
		sidePanel.setProperties(feature.properties);
	}

	/**
	 * Generate and return HTML content for display in the legend.
	 */
	getLegendContent() {
		throw new Error("The 'getLegendContent()' method must be implemented in a concrete subclass!");
	}

}