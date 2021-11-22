/**
 * This class uses the metadata discovered by the DataRegistry
 * to create MapBox layers for each data source.
 */
class LayerHandler {

    /**
     * DataRegistry instance.
     */
    _dataRegistry;

    /**
      * MapBox map
      */
    _map;


    /**
      * Initialise a new LayerHandler.
      * 
      * @param {*} dataRegistry DataRegistry
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

        let layers = [];
        fixedDataSets.forEach(dataSet => {
            layers.push(this.#addLayer(dataSet));
        });

        return layers;
    }

    /**
     * Adds the Additional Data sets represented by the input groups as
     * automatically generated MapBox layers.
     * 
     * @param {string[]} groups
     */
    addAdditionalLayers(groups) {
        let result = this._dataRegistry.getAdditionalGroup(groups);
        let layers = [];

        if(result != null) {
            let dataSets = result["dataSets"];

            for(var i = 0; i < dataSets.length; i++) {
                let dataSet = dataSets[i];
                if(!dataSet["locationType"]) continue;
                layers.push(this.#addLayer(dataSet));
            }
        }

        return layers;
    }

    /**
     * Given a single dataset, determine the correct type of layer to create,
     * create it, then return the MapBox id of that new layer.
     * 
     * @param {JSONObject} dataSet data set specification 
     * @returns layer name
     */
    #addLayer(dataSet) {
        let layerName = null;
        switch(dataSet["locationType"]) {
            case "point":
                layerName = this.#addPointLayer(dataSet);
                break;

            case "symbol":
                layerName = this.#addSymbolLayer(dataSet);
                break;

            case "fill":
            case "polygon":
                layerName = this.#addFillLayer(dataSet);
                break;

            case "extrusion":
                layerName = this.#addExtrusionLayer(dataSet);
                break;

            case "line":
                layerName= this.#addLineLayer(dataSet);
                break;
        }
        this.orderLayers();
        return [layerName, dataSet["locationType"]];
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
     * Adds a layer to show point location data.
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

    /**
     * Adds a layer to show symbol location data.
     * 
     * @param {JSONObject} dataSet 
     */
    #addSymbolLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        this._map.addLayer({
			id: layerName,
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
            type: 'symbol',
			layout: {
				'visibility': 'visible'
			},
			layout: {
				'icon-image': ["case", ["has", "icon-image"], ["get", "icon-image"], "circle-black"],
				'icon-size':  ["case", ["has", "icon-size"], ["get", "icon-size"], 1.0],
                'icon-allow-overlap': true,
                'icon-ignore-placement': true
			}
		});

        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }


    /**
     * Adds a layer to create fills for polygon location data.
     * 
     * @param {JSONObject} dataSet 
     */
    #addFillLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let backupFillColor = this.#getRandomColor();

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
                'fill-opacity': [
                    "case", 
                    ['boolean', ['feature-state', 'hover'], false],
                    0.50, 
                    0.33
                ]
			}
		});
        
        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }

    /**
     * Adds a layer to create fill extrusions for polygon location data.
     * 
     * @param {JSONObject} dataSet 
     */
    #addExtrusionLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        this._map.addLayer({
			id: layerName,
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
			type: 'fill-extrusion',
			layout: {
				'visibility': 'visible'
			},
			paint: {
                'fill-extrusion-base': ["case", ["has", "fill-extrusion-base"], ["get", "fill-extrusion-base"], 0],
                'fill-extrusion-height': ["case", ["has", "fill-extrusion-height"], ["get", "fill-extrusion-height"], 25],
                'fill-extrusion-opacity': 0.33,

                'fill-extrusion-color': [
                    "case", 
                    ['boolean', ['feature-state', 'hover'], false],
                    "hsl(200, 75%, 90%)",
                    ["case", ["has", "fill-extrusion-color"], ["get", "fill-extrusion-color"], "hsl(190, 25%, 25%)"]
                ]
			}
		});

        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }

     /**
     * Adds a layer to create lines for location data.
     * 
     * @param {JSONObject} dataSet 
     */
      #addLineLayer(dataSet) {
        let layerName = dataSet["name"];
        let sourceName = dataSet["name"];

        let backupColor = this.#getRandomColor();

        // Visible layer
        this._map.addLayer({
			id: layerName,
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
			type: 'line',
			layout: {
				'visibility': 'visible'
			},
			paint: {
                'line-color': ["case", ["has", "line-color"], ["get", "line-color"], backupColor],
                'line-opacity': ["case", ["has", "line-opacity"], ["get", "line-opacity"], 0.5],
                'line-width': ["case", ["has", "line-width"], ["get", "line-width"], 3]
			}
		});

        // Interaction layer
        this._map.addLayer({
			id: layerName + "_clickable",
			source: sourceName,
            metadata: {
                provider: "cmcl"
            },
			type: 'line',
			layout: {
				'visibility': 'visible'
			},
			paint: {
                'line-color': "#000000",
                'line-opacity': 0.0,
                'line-width': 10
			}
		});

        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }
    
    /**
     * Bump all point locations to the top of the stack, for better interactions
     * they should really be above any line or fill layers.
     */
    orderLayers() {
        var layers = this._map.getStyle().layers
        layers.forEach(layer => {

            if(layer["metadata"] && layer["metadata"]["provider"]) {
                let provider = layer["metadata"]["provider"];

                if(provider === "cmcl"){
                    if(layer["type"] == "circle" || layer["type"] === "symbol") {
                        this._map.moveLayer(layer["id"]);
                    }
                }
            }
        });
    }

    /**
     * Returns a random color.
     * 
     * @returns
     */
    #getRandomColor() {
        let hue = Math.floor(Math.random() * 12) * 30;
        let sat = 50 + Math.floor(Math.random() * 51);
        return "hsl(" + hue + ", " + sat + ", 50%)";
    }

    /**
     * Given a color, returns a darker version of it to use as outlines.
     * 
     * @param {string} fillColor 
     * @returns 
     */
    #getOutlineColor(fillColor) {
        return fillColor.replace("50%)", "35%)");
    }
}
// End of class.