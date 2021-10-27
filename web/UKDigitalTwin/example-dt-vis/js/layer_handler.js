/**
 * TODO
 */
class LayerHandler {

    /**
     * Colors for point locations.
     */
    #POINT_COLORS = ["#EF1919", "#1966EF", "#51EF19", "#EF19E8", "#EF8719", "#19EFBD", "#E6EF19"];

    /**
     * Outline colors for point locations.
     */
    #POINT_OUTLINES = ["#FFFFFF", "#000000"];

    /**
     * Colors for region fills.
     */
    #FILL_COLORS = ["#EF1919", "#1966EF", "#51EF19", "#EF19E8", "#EF8719", "#19EFBD", "#E6EF19"];

    /**
     * Outline colors for regions.
     */
    #FILL_OUTLINES = ["#B11616", "#114196", "#349512", "#920F8E", "#9F5C14", "#0D8D6F", "#949A13"];

    /**
     * DigitalTwinDataRegistry instance.
     */
    _dataRegistry;

    /**
      * MapBox map
      */
    _map;


    /**
      * Initialise a new DigitalTwinDataHandler.
      * 
      * @param {*} dataRegistry DigitalTwinDataRegistry
      * @param {*} map MapBox map 
      */
    constructor(dataRegistry, map) {
        this._dataRegistry = dataRegistry;
        this._map = map;
    }

    /**
     * Adds the Fixed Data sets as automatically generated
     * MapBox layers.
     */
    addFixedLayers() {
        let fixedMeta = this._dataRegistry.fixedMeta;
        let fixedDataSets = fixedMeta["dataSets"];

        let layerNames = [];

        fixedDataSets.forEach(fixedDataSet => {
            if(fixedDataSet["locationType"] === "point") {
                let layerName = this.#addPointLayer(fixedDataSet, false);
                layerNames.push(layerName);

            } else if(fixedDataSet["locationType"] === "polygon") {
                let layerName = this.#addPolygonLayer(fixedDataSet);
                layerNames.push(layerName);
            }
        });

        return layerNames;
    }

    /**
     * Adds the Additional Data sets represented by the input groups as
     * automatically generated MapBox layers.
     * 
     * @param {string[]} groups
     */
    addAdditionalLayers(groups) {
        let result = this._dataRegistry.getAdditionalGroup(groups);
        let layerNames = [];

        if(result != null) {

            let dataSets = result["dataSets"];

            for(var i = 0; i < dataSets.length; i++) {
                let dataSet = dataSets[i];
                if(!dataSet["locationType"]) continue;

                if(dataSet["locationType"] === "polygon") {
                    let layerName = this.#addPolygonLayer(dataSet);
                    layerNames.push(layerName);

                } else {
                    let layerName = this.#addPointLayer(dataSet, false);
                    layerNames.push(layerName);
                }
            }
        }

        return layerNames;
    }

    /**
     * 
     * @param {JSONObject} dataSet 
     */
    #addPointLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let backupFillColor = this.#getRandomColor();
        let backupStrokeColor = this.#getOutlineColor(backupFillColor);

        this._map.addLayer({
			id: layerName,
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
            type: 'circle',
			layout: {
				'visibility': 'visible'
			},
			paint: {
				'circle-radius': ["case", ["has", "circle-radius"], ["get", "circle-radius"], 5],
				'circle-color':  ["case", ["has", "circle-color"], ["get", "circle-color"], backupFillColor],
                'circle-opacity': ["case", ["has", "circle-opacity"], ["get", "circle-opacity"], 0.75],
				'circle-stroke-width': ["case", ["has", "circle-stroke-width"], ["get", "circle-stroke-width"], 1],
				'circle-stroke-color':  ["case", ["has", "circle-stroke-color"], ["get", "circle-stroke-color"], backupStrokeColor],
                'circle-stroke-opacity': ["case", ["has", "circle-stroke-opacity"], ["get", "circle-stroke-opacity"], 0.75]
			}
		});

        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }

    #getRandomColor() {
        let hue = Math.floor(Math.random() * 12) * 30;
        let sat = 50 + Math.floor(Math.random() * 51);
        return "hsl(" + hue + ", " + sat + ", 50%)";
    }

    #getOutlineColor(fillColor) {
        return fillColor.replace("50%)", "25%)");
    }

    /**
     * 
     * @param {JSONObject} dataSet 
     */
    #addPolygonLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let backupFillColor = this.#getRandomColor();
        let backupStrokeColor = this.#getOutlineColor(backupFillColor);

        this._map.addLayer({
			id: layerName,
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
			type: 'fill',
			layout: {
				'visibility': 'visible'
			},
			paint: {
                'fill-color': ["case", ["has", "fill-color"], ["get", "fill-color"], backupFillColor],
				'fill-opacity': ["case", ["has", "fill-opacity"], ["get", "fill-opacity"], 0.33],
                'fill-outline-color': ["case", ["has", "fill-outline-color"], ["get", "fill-outline-color"], backupStrokeColor],
			}
		});
        
        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }
    
    /**
     * Removes the MapBox layers corresponding to the Additional Data sets
     * represented by the input groups.
     * 
     * @param {string[]} groups
     */
    removeAdditionalLayers(groups) {
        let result = this._dataRegistry.getAdditionalGroup(groups);
        if(result != null) {
            let dataSets = result["dataSets"];

            for(var i = 0; i < dataSets.length; i++) {
                let dataSet = dataSets[i];
                if(!dataSet["locationFile"]) continue;

                let name = dataSet["name"];
                if(this._map.getLayer(name) != null) {
                    this._map.removeLayer(name);
                    console.log("INFO: Layer '" + name + "' has been removed.");
                }
            }
        }
    }

    /**
     * Finds layers that match the input type and were provided by CMCL (i.e. not included
     * with the MapBox style).
     * 
     * @param {*} layer 
     * @param {*} targetType 
     * @returns 
     */
      #findLayers(layer, targetType) {
        return layer.type === targetType && (layer.metadata != null && layer.metadata.provider === "cmcl");
    }

}