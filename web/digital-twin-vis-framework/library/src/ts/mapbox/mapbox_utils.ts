/**
 * Utilities specific to MapBox implementations
 */
class MapBoxUtils {

    /**
     * Returns true if the input feature is contained within a layer
     * created by CMCL (rather than an existing one from MapBox).
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
                if(!layer["metadata"]["attribution"] || layer["metadata"]["attribution"] !== "CMCL Innovations") {
                    return false;
                }

                if(!layer["metadata"]["clickable"]) {
                    return false;
                }
            }

            return true;

        } catch(error) {
            return false;
        }
    }

    /**
     * Generates and displays a descriptive popup on a MapBox map.
     * 
     * @param feature selected feature
     */
    public static showPopup(feature: Object) {
        let properties = feature["properties"];

        // Get feature details
        let name = properties["name"];
        if(name === null || name === undefined) return;

        let desc = properties["description"];
        if(desc === null && properties["desc"]) {
            desc = properties["desc"];
        }

        // Make HTML string
        let html = "<h3>" + name + "</h3>";
        if(desc !== undefined) html += desc; 

        // Add thumbnail if present
        if(properties["thumbnail"]) {
            html += "<br/><br/>";
            html += "<img class='thumbnail' src='" + properties["thumbnail"] + "'>";
        }

        // Show popup at the center of the feature
        let location = getCenter(feature);

        MapHandler_MapBox.POPUP.setLngLat(location)
            .setHTML(html)
            .addTo(MapHandler.MAP);
    }

    /**
     * 
     */
    public static async recurseFeatureNames(leafs: Array<Object>, features) {
        for(let feature of features) {
            if(MapBoxUtils.isCluster(feature)) {
                // Clustered point, get leafs
                let sourceName = feature["layer"]["source"];
                let source = MapHandler.MAP.getSource(sourceName);

                let result = await MapBoxUtils.getClusterLeaves(feature, 999, 0);
                MapBoxUtils.recurseFeatureNames(leafs, result);

            } else {
                leafs.push(feature);
            }
        }
    }

    /**
     * 
     */
    public static async getClusterLeaves(cluster, limit, offset) {
        let sourceName = cluster["layer"]["source"];
        let source = MapHandler.MAP.getSource(sourceName);

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
     * 
     */
    public static async isCluster(feature: Object) {
        return (feature["properties"] && feature["properties"]["point_count"]);
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
        for(let i = 0; i < layerIDs.length; i++) {
            let layerID = layerIDs[i];

            if(MapHandler.MAP.getLayer(layerID) != null) {
                return true;
            }
        }
        return false;
    }

    /**
     * 
     */
    public static anyLayersVisible(layerIDs: Array<String>) {
        for(let i = 0; i < layerIDs.length; i++) {
            let layerID = layerIDs[i];
            let prop = MapHandler.MAP.getLayoutProperty(layerID, "visibility");
            if(prop === "visible") return true;
        }
        return false;
    }


    /**
     * Change the underlying MapBox style.
     * 
     * @param {String} mode {"light", "dark", "satellite", "satellite-streets"}
     */
    public static  changeTerrain(mode) {
        if(mode === "light") {
            MapHandler.MAP.setStyle("mapbox://styles/mapbox/light-v10?optimize=true");
        } else if(mode === "dark") {
            MapHandler.MAP.setStyle("mapbox://styles/mapbox/dark-v10?optimize=true");
        } else if(mode === "outdoors") {
            MapHandler.MAP.setStyle("mapbox://styles/mapbox/outdoors-v11?optimize=true");
        } else if(mode === "blueprint") {
            MapHandler.MAP.setStyle("mapbox://styles/cmclinnovations-credo/ckzfn4jg3007x14l9zomsv7sd");
        } else if(mode === "satellite") {
            MapHandler.MAP.setStyle("mapbox://styles/mapbox/satellite-streets-v11?optimize=true");
        }

        // Store the current terrain as a global variable
        window.terrain = mode;
        
        // Hide default building outlines
        MapBoxUtils.hideBuildings();
    }

    /**
     * Hide building outlines provided by MapBox as these may conflict with custom
     * building data.
     */
    public static hideBuildings() {
        if(MapHandler.MAP == null) return;

        let ids = ["building", "building-outline", "building-underground"];
        ids.forEach(id => {
            if(MapHandler.MAP.getLayer(id) != null) MapHandler.MAP.setLayoutProperty(id, "visibility", "none");
        });
    }

    /**
     * Reset the camera to a default position.
     * 
     * @param {String} mode {"bird", "pitch"}
     */
    public static changeCamera(mode) {
        let mapOptions = DataUtils.getMapOptions(Manager.CURRENT_GROUP);

        if(mode === "bird") {
            MapHandler.MAP.flyTo({
                curve: 1.9,
                speed: 1.6,
                pitch: 0.0,
                bearing: mapOptions["bearing"],
                zoom: mapOptions["zoom"],
                center: mapOptions["center"]
            });

        } else if(mode === "pitch") {
            MapHandler.MAP.flyTo({
                curve: 1.9,
                speed: 1.6,
                pitch: 65,
                bearing: mapOptions["bearing"],
                zoom: mapOptions["zoom"],
                center: mapOptions["center"]
            });
        } 
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
				MapBoxUtils.updateTiltShift();
			});
			MapHandler.MAP.on("pitch", function() {
				MapBoxUtils.updateTiltShift();
			});
			MapBoxUtils.updateTiltShift();
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
     * Adds 3D terrain provided by MapBox. 
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
	 * Shows/hides place name labels supplied by MapBox.
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
	 * Show or hide a single (MapBox) layer on the map.
	 * 
	 * @param {String} layerID MapBox layer name.
	 * @param {boolean} visible desired visibility.
	 */
	public static toggleLayer(layerID, visible) {
		if(MapHandler.MAP.getLayer(layerID) == null) return;
		
        MapHandler.MAP.setLayoutProperty(
            layerID,
            "visibility",
            (visible ? "visible" : "none")
        );

        // Is there a corresponding _clickable layer?
        if(MapHandler.MAP.getLayer(layerID + "_clickable") != null) {
            MapHandler.MAP.setLayoutProperty(
                layerID + "_clickable",
                "visibility",
                (visible ? "visible" : "none")
            );
        }

        // Is there a corresponding _cluster layer?
        if(MapHandler.MAP.getLayer(layerID + "_cluster") != null) {
            MapHandler.MAP.setLayoutProperty(
                layerID + "_cluster",
                "visibility",
                (visible ? "visible" : "none")
            );
        }

        // Is there a corresponding _arrows layer?
        if(MapHandler.MAP.getLayer(layerID + "_arrows") != null) {
            MapHandler.MAP.setLayoutProperty(
                layerID + "_arrows",
                "visibility",
                (visible ? "visible" : "none")
            );
        }

        // Is there a corresponding -highlight layer?
        if(MapHandler.MAP.getLayer(layerID + "-highlight") != null) {
            MapHandler.MAP.setLayoutProperty(
                layerID + "-highlight",
                "visibility",
                (visible ? "visible" : "none")
            );
        }
	}


}