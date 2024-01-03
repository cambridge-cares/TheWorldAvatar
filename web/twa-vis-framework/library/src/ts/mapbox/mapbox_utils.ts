/**
 * Utilities specific to Mapbox implementations
 */
class MapboxUtils {

    /**
     * IRI of currently hovered feature.
     */
    public static HOVERED_IRI;

    /**
     * IRI of currently selected feature.
     */
    public static SELECTED_IRI;

    /**
     * Returns true if the input feature is contained within a layer
     * created by CMCL (rather than an existing one from Mapbox).
     * 
     * @param feature 
     * 
     * @returns true if from CMCL layer
     */
    public static isCMCLLayer(feature: Object) {
        try {
            let layer = feature["layer"];
            if(MapHandler.MAP.getLayoutProperty(layer["id"], "visibility") === "none") {
                return false;
            }

            if(!layer["metadata"]) {
                return false;
            } else {
                if(!layer["metadata"]["attribution"] || layer["metadata"]["attribution"] !== "CMCL") {
                    return false;
                }
            }

            return true;

        } catch(error) {
            return false;
        }
    }

    /**
     * Generates and displays a descriptive popup on a Mapbox map.
     * 
     * @param feature selected feature
     */
    public static showPopup(feature: Object) {
        if(MapboxUtils.isCluster(feature)) {
            MapboxUtils.showClusterPopup(feature);
            return;
        }

        // Get metadata
        let properties = feature["properties"];
        if(properties == null) return;

        // Update and show popup
        PopupHandler.updatePopup(properties);
        PopupHandler.setVisibility(true);
    }

    /**
     * Build a popup for cluster features.
     */
    private static showClusterPopup(feature: Object) {
        let name = "Multiple locations";
        let desc = `
            This feature represents a cluster of ` + feature["properties"]["point_count"] + 
            ` (or more) closely spaced, individual locations.<br/>Click to see details on the underlying locations.
        `;

        // Emulate feature metadata
        let properties = {
            "name": name,
            "desc": desc
        }

        // Update and show popup
        PopupHandler.updatePopup(properties);
        PopupHandler.setVisibility(true);
    }   

    /**
     * Recurse all features within a cluster feature.
     */
    public static async recurseFeatures(leafs: Array<Object>, features) {
        for(let i = 0; i < features.length; i++) {
            let feature = features[i];

            if(MapboxUtils.isCluster(feature)) {
                // Clustered point, get leafs
                let result = await MapboxUtils.getClusterLeaves(feature, feature["layer"]["source"], 999, 0) as Array<Object>;

                result.forEach(leaf => {
                    if(leaf["layer"] === null || leaf["layer"] === undefined) {
                        leaf["layer"] = feature["layer"];
                    }
                });

                MapboxUtils.recurseFeatures(leafs, result);
            } else {
                leafs.push(feature);
            }
        }
    }

    /**
     * Get the features from within a cluster.
     */
    public static async getClusterLeaves(cluster, sourceID, limit, offset) {
        let source = MapHandler.MAP.getSource(sourceID);

        return new Promise((resolve, reject) => {
            source.getClusterLeaves(cluster["id"], limit, offset, (error, leafs) => {
                if (error) {
                    reject(error);
                } else {
                    resolve(leafs);
                }
            });
        });
    };

    /**
     * True if the input feature is a cluster.
     */
    public static isCluster(feature: Object) {
        return (feature["properties"].hasOwnProperty("cluster") && feature["properties"]["cluster"]);
    }

    /**
     * Returns true if any of the input layer ids are layers that existing on 
     * the map (even if hidden).
     * 
     * @param {string[]} layerIDs mapbox layer ids
     *  
     * @returns true if any layers present
     */
    public static anyLayersAdded(layerIDs) {
        for(const element of layerIDs) {
            let layerID = element;

            if(MapHandler.MAP.getLayer(layerID) != null) {
                return true;
            }
        }
        return false;
    }

    /**
     * True if any layers are currently visible
     */
    public static anyLayersVisible(layerIDs: Array<string>) {
        for(const element of layerIDs) {
            let layerID = element;
            let prop = MapHandler.MAP.getLayoutProperty(layerID, "visibility");
            if(prop === "visible") return true;
        }
        return false;
    }

    /**
     * Returns true if the layer hosting the input feature is considered
     * clickable.
     * 
     * @param feature feature within layer.
     */
    public static isLayerClickable(feature) {
        let layer = Manager.DATA_STORE.getLayerWithID(feature["layer"]["id"]);
        if(layer == null) return false;

        let clickable = (layer.interactions === "all" || layer.interactions === "click-only");
        return clickable;
    }

    /**
     * Change the underlying Mapbox imagery style.
     * 
     * @param {String} mode terrain mode (i.e. "light", "dark" etc.)
     */
    public static  changeTerrain(mode) {
        // Skip if selecting the same again
        if(window.terrain.toUpperCase() === mode.toUpperCase()) return;

        // Get the imagery URL for the chosen setting.
        let imagerySettings = Manager.SETTINGS.getSetting("imagery");
        if(imagerySettings == null) return;

        // Update the imagery URL 
        let url = imagerySettings[mode];
        if(url == null) return;
        if(url.endsWith("_token=")) url += MapHandler.MAP_API;

        // Push new style URL to Mapbox map
        MapHandler.MAP.setStyle(url);
        MapHandler.MAP.setProjection({
            name: 'mercator'
        });

        // Store the current terrain as a global variable
        window.terrain = mode;
    }

    /**
     * Generates a JSON object defining the default imagery options if none is provided
     * by the developer in the settings.json file.
     */
    public static generateDefaultImagery() {
        let imagerySettings = {};

        // Add possible imagery options
        imagerySettings["Light"] = "mapbox://styles/mapbox/light-v11?optimize=true";
        imagerySettings["Dark"] = "mapbox://styles/mapbox/dark-v11?optimize=true";
        imagerySettings["Outdoors"] = "mapbox://styles/mapbox/outdoors-v12?optimize=true";
        imagerySettings["Satellite"] = "mapbox://styles/mapbox/satellite-streets-v12?optimize=true";

        // Set default imagery to Light
        imagerySettings["default"] = "Light";

        // Push settings
        Manager.SETTINGS.putSetting("imagery", imagerySettings);
    }

    /**
     * Reset the camera to default position.
     */
      public static resetCamera() {
        let mapOptions = MapHandler.MAP_OPTIONS;
        if(mapOptions == null) return;

        MapHandler.MAP.flyTo({
            curve: 1.9,
            speed: 1.6,
            pitch: mapOptions["pitch"],
            bearing: mapOptions["bearing"],
            zoom: mapOptions["zoom"],
            center: mapOptions["center"]
        });
    }

    /**
	 * Enable (or disable) depth of field effect.
	 * 
	 * @param {Boolean} enabled depth of field state. 
	 */
	public static setTiltshift(enabled) {
		var tiltShiftComponent = document.getElementById("tiltShift");
        if(tiltShiftComponent === null || tiltShiftComponent === undefined) return;

		tiltShiftComponent.style.display = (enabled) ? "block" : "none";

		if(enabled) {
			var self = this;
			MapHandler.MAP.on("zoom", function() {
				MapboxUtils.updateTiltShift();
			});
			MapHandler.MAP.on("pitch", function() {
				MapboxUtils.updateTiltShift();
			});
			MapboxUtils.updateTiltShift();
		}
	}

    /**
	 * Updates the tiltshift effect based on the current zoom and pitch.
	 */
	public static updateTiltShift() {
		var tiltShiftComponent = document.getElementById("tiltShift");
        if(tiltShiftComponent === null || tiltShiftComponent === undefined) return;

		if(tiltShiftComponent.style.display === "block") {
			var blurAmount = 5;

			var pitch = MapHandler.MAP.getPitch();
			var zoom = MapHandler.MAP.getZoom() / 20;
			var fractionPitched = zoom * (pitch / 90);

            var topStart = "black " + (5 * fractionPitched) + "%";
			var topEnd = "rgba(0, 0, 0, 0) " +  (60 * fractionPitched) + "%";
			var bottomStart = "rgba(0, 0, 0, 0) " + (100 - (15 * fractionPitched)) + "%";
			var bottomEnd ="rgba(0, 0, 0, 0.5) 100%";

            // @ts-ignore
			tiltShiftComponent.style.backdropFilter = "blur(" + blurAmount + "px)";
            // @ts-ignore
			tiltShiftComponent.style.webkitBackdropFilter = "blur(" + blurAmount + "px)";
            // @ts-ignore
			tiltShiftComponent.style.webkitMaskImage = "linear-gradient(" + topStart + ", " + topEnd + ", " + bottomStart +  ", " + bottomEnd + ")";
			// @ts-ignore
            tiltShiftComponent.style.maskImage = "linear-gradient(" + topStart + ", " + topEnd + ", " + bottomStart +  ", " + bottomEnd + ")";
		}
	}

    /**
     * Adds 3D terrain provided by Mapbox. 
     * WARNING: This may not be compatible with 3D building data unless the
     * building's base height has been set correctly.
     */
    public static set3DTerrain(enabled) {
        if(enabled) {
            MapHandler.MAP.addSource('mapbox-3d-terrain', {
                type: 'raster-dem',
                url: 'mapbox://mapbox.mapbox-terrain-dem-v1',
                tileSize: 512,
                maxzoom: 14
            });
            MapHandler.MAP.setTerrain({
                source: 'mapbox-3d-terrain', 
                exaggeration: 1.5
            });
        } else {
            MapHandler.MAP.setTerrain(null);
            if(MapHandler.MAP.getSource("mapbox-3d-terrain") != null) MapHandler.MAP.removeSource('mapbox-3d-terrain');
        }
    }

    /**
	 * Shows/hides place name labels supplied by Mapbox.
	 * 
	 * @param {Boolean} enabled 
	 */
	public static setPlacenames(enabled) {
		let ids = ["road-number-shield", "road-label", "road-intersection", "waterway-label", "natural-line-label",
		"natural-point-label", "water-line-label", "water-point-label", "poi-label", "airport-label", "settlement-subdivision-label",
		"settlement-minor-label", "settlement-major-label", "settlement-label", "state-label", "country-label", "road-oneway-arrow-blue", 
		"road-oneway-arrow-white", "transit-label"]

        let layers = MapHandler.MAP.getStyle().layers;
		layers.forEach(layer => {
            let applicable = ids.includes(layer["id"]) || (window.terrain === "satellite" && layer["id"].startsWith("road-"));

            if(applicable) {
                MapHandler.MAP.setLayoutProperty(
                    layer["id"],
                    "visibility",
                    (enabled ? "visible" : "none")
                );
            } 
		});
	}

    /**
	 * Show or hide a single (Mapbox) layer on the map.
	 * 
	 * @param {String} layerID Mapbox layer name.
	 * @param {boolean} visible desired visibility.
	 */
	public static toggleLayer(layerID, visible) {
        // Get the original layer definition
        let layerObject = Manager.DATA_STORE.getLayerWithID(layerID);
        if(layerObject == null) return;

        // Skip if setting to the same value
        if(layerObject.getVisibility() === visible) return;

        // Push the visibility to the Mapbox map
        MapHandler.MAP.setLayoutProperty(
            layerID,
            "visibility",
            (visible ? "visible" : "none")
        );

        // Update the cached state within the layer definition
        layerObject.cacheVisibility(visible);
        console.log("Storing visibility for layer " + layerID + ", is now " + visible);
	}

    /**
     * Given a list of features, this returns a new list without any
     * duplicates (as defined by matching IDs).
     * 
     * @param features input feature list.
     * 
     * @returns deduplicated feature list.
     */
    public static deduplicate(features: Object[]) {
        let result = [];

        features.forEach(feature => {
            let id = feature["id"];

            let match = result.find(entry => entry["id"] === id);
            if(match === null || match === undefined) {
                result.push(feature);
            }
        });

        return result;
    }

    /**
     * Load search terms and populate the drop down box.
     */
    public static loadSearchTerms() {
        let terms = Manager.SETTINGS.getSetting("search");
        if(terms == null) {
            terms = {};
            terms["Name"] = "name";
        }

        let searchSelect = document.getElementById("search-select");
        for (const [key, value] of Object.entries(terms)) {
            let option = document.createElement("option");
            option.text = key;

            // @ts-ignore
            option.value = value;

            // @ts-ignore
            searchSelect.add(option);
        }
    }

    /**
     * Updates certain layers style and filter to inject (or clear) any
     * IRIs for the currently hovered or selected feature. Setting one of
     * the input IRIs to null will clear that parameter (i.e. revert back
     * to the placeholder string).
     * 
     * @param hoveredIRI IRI of hovered feature (can be null).
     * @param selectedIRI IRI of selected feature (can be null).
     */
    public static updateStyleFilterInjections(hoveredIRI, selectedIRI) {
        if(hoveredIRI === MapboxUtils.HOVERED_IRI && selectedIRI === MapboxUtils.SELECTED_IRI) return;

        let layers = Manager.DATA_STORE.getLayerList();
        layers.forEach(layer => {

            if(hoveredIRI !== MapboxUtils.HOVERED_IRI) {
                // Hover state changed, update properties
                MapboxUtils.handleInjections(
                    layer,
                    "hover",
                    (hoveredIRI == null) ? "[HOVERED-IRI]" : hoveredIRI,
                    "[HOVERED-IRI]"
                )
            }

            if(selectedIRI !== MapboxUtils.SELECTED_IRI) {
                // Selection state changed, update properties
                MapboxUtils.handleInjections(
                    layer,
                    "select",
                    (selectedIRI == null) ? "[SELECTED-IRI]" : selectedIRI,
                    "[SELECTED-IRI]"
                )
            }
        });

        // Cache the IRIs as we'll need to remove them from filters later.
        MapboxUtils.HOVERED_IRI = hoveredIRI;
        MapboxUtils.SELECTED_IRI = selectedIRI;
    }


    /**
     * Injects/clears the IRIs of hovered or selected features from
     * the style of the layer within the input ID.
     * 
     * @param layerObject target layer.
     * @param injectionType type of injection ("hover", "select").
     * @param newIRI IRI value to inject (null to clear).
     * @param oldIRI IRI value to replace (could be null).
     */
    private static handleInjections(layerObject, injectionType, newIRI, oldIRI) {
        // Skip is map is still loading the layer
        if(MapHandler.MAP.getLayer(layerObject.id) == null) return;

        let paintProps = layerObject.getCachedProperties(injectionType, "paint");
        if(paintProps != null) {
            for (const [key, value] of Object.entries(paintProps)) {
                let newValue = MapboxUtils.updateProperty(value, newIRI, oldIRI);
                MapHandler.MAP.setPaintProperty(layerObject.id, key, newValue);
            }
        }

        let layoutProps = layerObject.getCachedProperties(injectionType, "layout");
        if(layoutProps != null) {
            for (const [key, value] of Object.entries(layoutProps)) {
                let newValue = MapboxUtils.updateProperty(value, newIRI, oldIRI);
                MapHandler.MAP.setLayoutProperty(layerObject.id, key, newValue);
            }
        }

        let filter = layerObject.getCachedProperties(injectionType, "filter");
        if(filter != null) {
            let newValue = MapboxUtils.updateProperty(filter, newIRI, oldIRI);
            MapHandler.MAP.setFilter(layerObject.id, newValue);
        }
    }

    /**
     * Handles injection for a single property/filter.
     * 
     * @param property property (JSON Object);
     * @param newIRI IRI to be injected (null to clear);
     * @param oldIRI IRI/placeholder to be replaced.
     */
    private static updateProperty(property, newIRI, oldIRI) {
        if(newIRI === oldIRI) return property;

        let propertyStr = JSON.stringify(property);
        propertyStr = propertyStr.replaceAll(oldIRI, newIRI);
        return JSON.parse(propertyStr);
    }

}