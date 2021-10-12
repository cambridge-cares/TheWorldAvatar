/**
 * Example concrete instance of a DigitalTwinModule.
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
			data: "example/housing.geojson"
		});
		console.log("INFO: 'housing-data' source has been added.");

		this._map.addSource("admin-data", {
			type: "geojson",
			data: "example/authorities.geojson"
		});
		console.log("INFO: 'admin-data' source has been added.");
	}

	/**
	 * Generate and add layers to the MapBox map.
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
				'fill-color': '#36EAF4',
				'fill-opacity': [
					'case',
					['boolean', ['feature-state', 'hover'], false],
					0.75,
					0.25
				]
			}
		});
		this.addMouseEffects('admin-fill', true, true, true, false);
		console.log("INFO: 'admin-fill' layer has been added.");

		this._map.addLayer({
			id: 'admin-outline',
			source: 'admin-data',
			type: 'line',
			layout: {
				visibility: 'none'
			},
			paint: {
				'line-color': '#259CA3',
				'line-width': 1
			}
		});
		console.log("INFO: 'admin-outline' layer has been added.");

		// Register each MapBox layer separately under a single heading
		this.registerLayerGroup("Housing Board Area", ["housing-layer-fill", "housing-layer-outline"], true, "Regions");
		this.registerLayerGroup("Administrative Units", ["admin-fill", "admin-outline"], false, "Regions");
	}

	/**
	 * @param {*} layerName 
	 * @param {*} feature 
	 */
	onMouseMove(layerName, feature) {
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
	 * 
	 * @param {*} popup 
	 * @param {*} layerName 
	 * @param {*} feature 
	 */
	onMouseEnter(popup, layerName, feature) {
		// Needs to be implemented in your concrete module class.
	}

	/**
	 * 
	 * @param {*} popup 
	 * @param {*} layerName 
	 * @param {*} feature 
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