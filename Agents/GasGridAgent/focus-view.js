
/**
 * TODO - Write documentation.
 */
class FocusView {

    // MapBox map.
    _map;

    /**
     * Initialise a new FocusView instance.
     * 
     * @param {MapBox map} map current map object 
     */
    constructor(map) {
        this._map = map;
    }

    /**
     * Create and add the background overlay layer.
     * 
     * @param {String} color optional hex color (defaults to black) 
     */
    createOverlay(color = "#FFFFFF") {
        if(this._map.getLayer("focus-overlay") == null) {
            this._map.addLayer({
                "id": "focus-overlay",
                "type": "background",
                "metadata": {
                    "provider": "cmcl"
                },
                "layout": {
                    "visibility": "visible"
                },
                "paint": {
                "background-color": color,
                "background-opacity": 0.66
                }
            });
        }
    }

    /**
     * Given a key/value collection of layer names and permitted feature IDs within them,
     * this method duplicates those layers and sets a filter on the new duplication that 
     * will only show features with the permitted IDs.
     * 
     * @param {{String, Integer[]}} allowedIDs layer names and the permitted feature IDs within them
     */
    duplicateLayers(allowedIDs) {
        for (const [layerName, ids] of Object.entries(allowedIDs)) {
            // Get existing layer
            let existingLayer = this._map.getLayer(layerName);
            if(existingLayer == null) continue;
            console.log(existingLayer);

            // Clone that layer with a new ID
            let newID = existingLayer["id"] + "-focus-clone";
            if(this._map.getLayer(newID) == null) {
                let cloneLayer = this.#cloneLayer(newID, existingLayer);
                this._map.addLayer(cloneLayer);
            }

            // TEST
            //this._map.setPaintProperty(cloneLayer["id"], "circle-color", "#00FF00");
            // TEST

            // Filter so that only features with IDs within allowedIDs are shown
            let literal = ["literal", ids];
            let filter = ["in", ["id"], literal];
            this._map.setFilter(newID, filter);
        }
    }

    /**
     * Given an existing MapBox layer object, this creates a JSON object that
     * will can be used to create a new, nearly-identical layer.
     * 
     * @param {MapBox Layer} existingLayer 
     * @returns cloned layer JSON
     */
    #cloneLayer(newID, existingLayer) {

        // Clone the existing paint properties. This is dodgy as MapBox gives no way
        // to retrieve these other than accessing a private variable. In future, we 
        // should cache the settings used to create layers then we can clone those.
        let paint = {};
        for (var [key, value] of Object.entries(existingLayer.paint._values)) {
            try {
                paint[key] = this._map.getPaintProperty(existingLayer.id, key);
            } catch(error) {
                // Ignore and move on to next property
            }
        }

        // Clone the existing layout properties. This is dodgy as MapBox gives no way
        // to retrieve these other than accessing a private variable. In future, we 
        // should cache the settings used to create layers then we can clone those.
        let layout = {};
        for (var [key, value] of Object.entries(existingLayer.layout._values)) {
            try {
                layout[key] = this._map.getLayoutProperty(existingLayer.id, key);
            } catch(error) {
                // Ignore and move on to next property
            }
        }

        // Required properties
        let newLayer = {
            "id": newID,
            "type": existingLayer["type"],
            "source": existingLayer["source"]
        };
        if(Object.keys(layout).length != 0) newLayer["layout"] = layout;
        if(Object.keys(paint).length != 0) newLayer["paint"] = paint;

        // Properties that may have been set.
        if(existingLayer["filter"] != null) newLayer["filter"] = existingLayer["filter"];
        if(existingLayer["metadata"] != null) newLayer["metadata"] = existingLayer["metadata"];
        if(existingLayer["sourceLayer"] != null) newLayer["sourceLayer"] = existingLayer["sourceLayer"];
        if(existingLayer["minzoom"] != null) newLayer["minzoom"] = existingLayer["minzoom"];
        if(existingLayer["maxzoom"] != null) newLayer["maxzoom"] = existingLayer["maxzoom"];
        if(existingLayer["visibility"] != null) newLayer["visibility"] = existingLayer["visibility"];
        return newLayer;
    }


}