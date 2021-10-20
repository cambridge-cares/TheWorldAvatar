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

        fixedDataSets.forEach(fixedDataSet => {
            if(fixedDataSet["locationType"] === "point") {
                this.#addPointLayer(fixedDataSet, false);
            } else if(fixedDataSet["locationType"] === "polygon") {
                this.#addPolygonLayer(fixedDataSet);
            }
        });
    }

    /**
     * Adds the Additional Data sets represented by the input groups as
     * automatically generated MapBox layers.
     * 
     * @param {string[]} groups
     */
    addAdditionalLayers(groups) {
        let result = this._dataRegistry.getAdditionalGroup(groups);

        if(result != null) {
            let group = result["group"]; 

            if(group["locationType"] === "point") {
                this.#addPointLayer(group, false);
            } else if(group["locationType"] === "polygon") {
                this.#addPolygonLayer(group);
            }
        }
    }

    /**
     * 
     * @param {JSONObject} dataSet 
     */
    #addPointLayer(dataSet, isDarkStyle) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let existingPoints = this._map.getStyle().layers.filter(
            layer => this.#findLayers(layer, "circle")
        ).length;

        let pointColor = this.#POINT_COLORS[existingPoints % this.#POINT_COLORS.length];
        let pointOutline = (isDarkStyle) ? this.#POINT_OUTLINES[0] : this.#POINT_OUTLINES[1];

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
				'circle-radius': 5,
				'circle-color': pointColor,
				'circle-stroke-width': 1,
				'circle-stroke-color': pointOutline,
			}
		});

        //console.log(this._map.getStyle().layers);
        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
    }

    /**
     * 
     * @param {JSONObject} dataSet 
     */
    #addPolygonLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let existingFills = this._map.getStyle().layers.filter(
            layer => this.#findLayers(layer, "fill")
        ).length;

        let regionColor = this.#FILL_COLORS[existingFills % this.#FILL_COLORS.length];
        let regionOutline = this.#FILL_OUTLINES[existingFills % this.#FILL_OUTLINES.length];

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
                'fill-color': regionColor,
				'fill-opacity': 0.33,
                'fill-outline-color': regionOutline
			}
		});
        
        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
    }
    
    /**
     * Remove all layers currently on the map that use the input source name.
     * 
     * @param {*} sourceName source name
     */
    removeLayersWithSource(sourceName) {
        let allLayers = this._map.getStyle().layers;

        for(var i = (layers.length - 1); i >= 0; i--) {
            let layerSource = allLayers[i]["source"];

            if(layerSource === sourceName) this._map.removeLayer(allLayers[i]["id"]);
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