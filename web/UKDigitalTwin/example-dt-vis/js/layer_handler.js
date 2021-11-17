/**
 * This class uses the metadata discovered by the DataRegistry
 * to create MapBox layers for each data source.
 */
class LayerHandler {

    /**
      * MapBox map
      */
    _map;


    /**
      * Initialise a new LayerHandler.
      * 
      * @param {Object} map MapBox map 
      */
    constructor(map) {
        this._map = map;
    }

    /**
     * Adds a new layer to the MapBox map as defined by the
     * input metadata.
     * 
     * @param {JSONObject} dataSet metadata for single dataset.
     */
    addLayer(dataSet) {
        switch(dataSet["locationType"]) {
            default:
            case "point":
                this.#addPointLayer(dataSet);
                break;

            case "symbol":
                this.#addSymbolLayer(dataSet);
                break;

            case "fill":
            case "polygon":
                this.#addFillLayer(dataSet);
                break;

            case "extrusion":
                this.#addExtrusionLayer(dataSet);
                break;

            case "line":
                this.#addLineLayer(dataSet);
                break;
        }

        // Order the layers so that points are on top.
        this.#orderLayers();
    }

    /**
     * Removes all layers with the provider:cmcl metadata.
     */
    removeLayers() {
        var layers = this._map.getStyle().layers;

        for(var i = (layers.length - 1); i >= 0; i--) {
            let layer = layers[i];
            if(layer["metadata"] && layer["metadata"]["provider"] && layer["metadata"]["provider"] === "cmcl") {
                this._map.removeLayer(layer["id"]);
            }
        }

        console.log("INFO: Removed all Layers.");
    }

    /**
     * Adds a layer to render a sky effect.
     */
    addSkyLayer() {
        this._map.addLayer({
            'id': 'sky',
            'type': 'sky',
            'paint': {
                'sky-opacity': ['interpolate', ['linear'], ['zoom'], 0, 0, 5, 0.3, 8, 1 ],
                'sky-type': 'atmosphere',
                'sky-atmosphere-sun-intensity': 5
            }
        });

        this.setSunPosition(("dark" === DT.terrain) ? "sunsetStart" : "solarNoon");
        console.log("INFO: Special sky layer has been added.");
    }

    /**
     * Sets the position of the sun for the sky layer
     * 
     * @param {String} date 
     */
    setSunPosition(date) {
        var sunPositions = SunCalc.getTimes(
            Date.now(),
            this._map.getCenter().lat,
            this._map.getCenter().lng
        );
        if(date != null) date = sunPositions[date];

        var center = this._map.getCenter();
        var sunPos = SunCalc.getPosition(
            date || Date.now(),
            center.lat,
            center.lng
        );

        var sunAzimuth = 180 + (sunPos.azimuth * 180) / Math.PI;
        var sunAltitude = 90 - (sunPos.altitude * 180) / Math.PI;
        this._map.setPaintProperty('sky', 'sky-atmosphere-sun', [sunAzimuth, sunAltitude]);
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
				'circle-radius': ['interpolate', ['linear'], ['zoom'], 10, 3, 15, 10],
                'circle-opacity': 1.0,
				'circle-color':  ["case", ["has", "circle-color"], ["get", "circle-color"], backupFillColor],
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
                'icon-size':  ['interpolate', ['linear'], ['zoom'], 10, 0.33, 15, 0.66],
				'icon-image': ["case", ["has", "icon-image"], ["get", "icon-image"], "circle-black"],
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
                'line-width': ['interpolate', ['linear'], ['zoom'], 10, 5, 15, 12.5]
			}
		});

        console.log("INFO: Added '" + layerName + "' layer to MapBox.");
        return layerName;
    }
    
    /**
     * Bump all point locations to the top of the stack, for better interactions
     * they should really be above any line or fill layers.
     */
    #orderLayers() {
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