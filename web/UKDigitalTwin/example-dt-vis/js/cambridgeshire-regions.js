/**
 * Example concrete instance of a DigitalTwinModule.
 * 
 * This module handles loading data sources, creating layers, and handling
 * event logic for a series of example regions with Cambridgeshire.
 */
export class CambridgeshireRegionModule extends DigitalTwinModule {

	_hoveredRegionID = null;

	/**
	 * Initialise a new CambridgeshireRegionModule.
	 */
	constructor() {
		super("Cambridgeshire Regions");
	}
	
	/**
	 * Load and add data sources to the MapBox map.
	 */
	 addSources() {
		this._map.addSource("housing-data", {
			type: "geojson",
			data: "./data/housing.geojson"
		});
		console.log("INFO: 'housing-data' source has been added.");

		this._map.addSource("admin-data", {
			type: "geojson",
			data: "./data/authorities.geojson"
		});
		console.log("INFO: 'admin-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map. Mouse effects can 
	 * also be turned on here by calling the addMouseEffects() method.
	 */
	addLayers() {
		this._map.addLayer({
			id: 'housing-layer-fill',
			source: 'housing-data',
			type: 'fill',
			layout: {
				visibility: 'visible'
			},
			paint: {
				'fill-color': '#36EAF4',
				'fill-opacity': 0.3
			}
		});
		console.log("INFO: 'housing-layer-fill' layer has been added.");

		this._map.addLayer({
			id: 'housing-layer-outline',
			source: 'housing-data',
			type: 'line',
			layout: {
				visibility: 'visible'
			},
			paint: {
				'line-color': '#259CA3',
				'line-width': 1
			}
		});
		console.log("INFO: 'housing-layer-outline' layer has been added.");

		this._map.addLayer({
			id: 'admin-fill',
			source: 'admin-data',
			type: 'fill',
			layout: {
				visibility: 'none'
			},
			paint: {
				'fill-color': '#36f475',
				'fill-opacity': [
					'case',
					['boolean', ['feature-state', 'hover'], false],
					0.75,
					0.25
				]
			}
		});
		console.log("INFO: 'admin-fill' layer has been added.");

		this._map.addLayer({
			id: 'admin-outline',
			source: 'admin-data',
			type: 'line',
			layout: {
				visibility: 'none'
			},
			paint: {
				'line-color': '#36a325',
				'line-width': 1
			}
		});
		console.log("INFO: 'admin-outline' layer has been added.");
	}

	/**
	 * Fires when a mouse enters a plotted region. The logic that runs here
	 * will likely be specific to your data sources, so you will need to 
	 * determine what data to show/logic to run here yourself.
	 * 
	 * @param {?} popup popup element
	 * @param {String} layerName name of layer containing location
	 * @param {JSONObject} feature triggered feature
	 * @param {MouseEvent} event mouse event
	 */
	onMouseMove(popup, layerName, feature, event) {
		if(layerName === "admin-fill") {

			if(this._hoveredRegionID !== null) {
				this._map.setFeatureState(
					{ source: "admin-data", id: this._hoveredRegionID },
					{ hover: false }
				);
			}

			this._hoveredRegionID = feature.id;
			this._map.setFeatureState(
				{ source: "admin-data", id: this._hoveredRegionID },
				{ hover: true }
			);
		}
	}

	/**
	 * Fires when a mouse leaves a plotted location. This example just resets
	 * the hoveredRegionID, but data/layer specific actions could be taken here.
	 * 
	 * @param {?} popup popup element
	 * @param {String} layerName name of layer containing location
	 */
	onMouseExit(popup, layerName) {
		if(layerName === "admin-fill") {

			if (this._hoveredRegionID !== null) {
				this._map.setFeatureState(
					{ source: 'admin-data', id: this._hoveredRegionID },
					{ hover: false }
				);
			}
			this._hoveredRegionID  = null;
		}
	}


	/**
	 * Generate and return HTML content for display in the legend.
	 */
	getLegendContent() {
		var legendContent = `
			<img src="./img/legend-regions.svg" width="100" height="100" />
		`;
		return legendContent;
	}

}
// End of class.