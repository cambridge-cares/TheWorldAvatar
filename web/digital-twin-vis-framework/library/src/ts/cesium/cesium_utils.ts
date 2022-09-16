 /**
 * Utilities specific to MapBox implementations
 */
class CesiumUtils {
    
    /**
     * Silhouette outlines
     */
    private static OUTLINE_BLUE;
    private static OUTLINE_GREEN;

    /**
     * Returns the visibility state of the layer with the input ID.
     * 
     * @param layerID ID of layer
     * @returns visibility
     */
    public static isVisible(layerID: string): boolean {
        let dataSources = MapHandler_Cesium.DATA_SOURCES[layerID];
        if(dataSources === null || dataSources === undefined) return false;

        for(let i = 0; i < dataSources.length; i++) {
            let dataSource = dataSources[i];
            if(dataSource.show === false) {
                return false;
            }
        }
        return true;
    }

    /**
	 * Show or hide a single (MapBox) layer on the map.
	 * 
	 * @param {String} layerID MapBox layer name.
	 * @param {boolean} visible desired visibility.
	 */
    public static toggleLayer(layerID, visible) {
        // Get sources of data for this layer
        let dataSources = MapHandler_Cesium.DATA_SOURCES[layerID];

        if(dataSources !== null) {
            for(let i = 0; i < dataSources.length; i++) {
                let dataSource = dataSources[i];

                if(dataSource instanceof Cesium.WebMapServiceImageryProvider) {
                    // 2D data, need to find imageryLayers using this provider
                    let layers = MapHandler.MAP.imageryLayers;
                    
                    for(let i = 0; i < layers.length; i++) {
                        if(layers.get(i).imageryProvider === dataSource) {
                            layers.get(i).show = visible;
                        }
                    }
                } else {
                    // 3D data
                    dataSource.show = visible;
                }
            }
        } 
       
        MapHandler.MAP.scene.requestRender();
    }

    /**
     * Change the underlying MapBox style.
     * 
     * @param {String} mode {"light", "dark", "satellite", "satellite-streets"}
     */
    public static changeTerrain(mode) {
        // Find existing base layer
        let baseLayer = null;
        for(let i = 0; i < MapHandler.MAP.imageryLayers.length; i++) {

            let layer = MapHandler.MAP.imageryLayers.get(i);
            if(layer.isBaseLayer()) {
                baseLayer = layer;
                break;
            }
        }

        // Bug out if not found
        if(baseLayer === null || baseLayer === undefined) {
            console.error("Could not identify base layer!");
            return;
        }

        // Remove base layer
        MapHandler.MAP.imageryLayers.remove(baseLayer, true);

        // Start of URL for imagery tiles
        let tileURL = "https://api.mapbox.com/styles/v1/" + MapHandler.MAP_USER + "/";
        switch(mode.toLowerCase()) {
            default:
            case "light":
                tileURL += "cl6p66equ000314pj8v0p60ub";
            break;
            case "dark":
                tileURL += "cl6owj7v2000415q1oj9aq5zq";
            break;
            case "outdoors":
                tileURL += "cl6p69s56000t14p47gpfhg1o";
            break;
            case "satellite":
                tileURL += "cl6p6j16m000415tj1s4wtfi8";
            break;
        }

        // Finish tile URL
        tileURL += "/tiles/256/{z}/{x}/{y}?access_token=" + MapHandler.MAP_API;

        // Add our own imagery provider
        let imageryProvider = new Cesium.UrlTemplateImageryProvider({
            url: tileURL,
            credit: "mapbox"
        });
        MapHandler.MAP.scene.imageryLayers.addImageryProvider(imageryProvider, 0);
        MapHandler.MAP.scene.requestRender();
    }

    /**
     * Reset the camera to a default position.
     * 
     * @param {String} mode {"bird", "pitch"}
     */
    public static changeCamera(mode) {
        let mapOptions = MapHandler.MAP_OPTIONS;

        if(mode === "bird") {
            MapHandler.MAP.camera.flyTo({
                // @ts-ignore
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    // @ts-ignore
                    heading: Cesium.Math.toRadians(0),
                    // @ts-ignore
                    pitch: Cesium.Math.toRadians(-90),
                    // @ts-ignore
                    roll: Cesium.Math.toRadians(0)
                }
            });

        } else if(mode === "pitch") {
            MapHandler.MAP.camera.flyTo({
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    heading: Cesium.Math.toRadians(30),
                    pitch: Cesium.Math.toRadians(-45),
                    roll: Cesium.Math.toRadians(0)
                }
            });
        } 
    }

    /**
     * Enables hover-over silhouettes for 3D entities.
     * 
     * This is copied from the "3D Tiles Feature Picking" example on Cesium Sandcastle.
     */
    public static enableSilhouettes() {
        if (!Cesium.PostProcessStageLibrary.isSilhouetteSupported(MapHandler.MAP.scene)) return;

        // Information about the currently selected feature
        const selected = {
            feature: undefined,
            originalColor: new Cesium.Color(),
        };

        CesiumUtils.OUTLINE_BLUE = Cesium.PostProcessStageLibrary.createEdgeDetectionStage();
        CesiumUtils.OUTLINE_BLUE.uniforms.color = Cesium.Color.CORNFLOWERBLUE;
        CesiumUtils.OUTLINE_BLUE.uniforms.length = 0.01;
        CesiumUtils.OUTLINE_BLUE.selected = [];

        CesiumUtils.OUTLINE_GREEN = Cesium.PostProcessStageLibrary.createEdgeDetectionStage();
        CesiumUtils.OUTLINE_GREEN.uniforms.color = Cesium.Color.MEDIUMSEAGREEN  ;
        CesiumUtils.OUTLINE_GREEN.uniforms.length = 0.01;
        CesiumUtils.OUTLINE_GREEN.selected = [];

        MapHandler.MAP.scene.postProcessStages.add(
            Cesium.PostProcessStageLibrary.createSilhouetteStage([
                CesiumUtils.OUTLINE_BLUE,
                CesiumUtils.OUTLINE_GREEN,
            ])
        );

        // Silhouette a feature blue on hover.
        MapHandler.MAP.screenSpaceEventHandler.setInputAction(function onMouseMove(movement) {
                // If a feature was previously highlighted, undo the highlight
                CesiumUtils.OUTLINE_BLUE.selected = [];

                // Pick a new feature
                const pickedFeature = MapHandler.MAP.scene.pick(movement.endPosition);

                if (!Cesium.defined(pickedFeature)) return;

                // Highlight the feature if it's not already selected.
                if (pickedFeature !== selected.feature) {
                    CesiumUtils.OUTLINE_BLUE.selected = [pickedFeature];
                }
            },
            Cesium.ScreenSpaceEventType.MOUSE_MOVE
        );
    }

    /**
     * Highlights the selected 3D entity with it's own silhouette.
     * 
     * @param feature selected feature
     * @param event mouse click event
     */
    public static setSelectedSilhouette(feature, event) {
        if (!Cesium.PostProcessStageLibrary.isSilhouetteSupported(MapHandler.MAP.scene)) return;

        // If a feature was previously selected, undo the highlight
        CesiumUtils.OUTLINE_GREEN.selected = [];

        // Select the feature if it's not already selected
        if (CesiumUtils.OUTLINE_GREEN.selected[0] === feature) return;

        // Save the selected feature's original color
        const highlightedFeature = CesiumUtils.OUTLINE_GREEN.selected[0];
        if (feature === highlightedFeature) {
            CesiumUtils.OUTLINE_GREEN.selected = [];
        }

        // Highlight newly selected feature
        CesiumUtils.OUTLINE_GREEN.selected = [feature];
    }

    /**
     * Given a mouse event, this utils method returns the top-level feature under the mouse (if any is present).
     * 
     * @param event mouse location
     * @param callback callback that feature will be passed to
     * 
     * @returns resulting feature (or null);
     */
    public static getFeature(event, callback) {
        if(!callback) {
            throw "Callback function is required!";
        }

        // Get the feature at the click point
        const feature = MapHandler.MAP.scene.pick((!event.position) ? event.endPosition : event.position);

        if(feature === null || feature === undefined) {
            // Probably a WMS feature, need to get info differently
            var pickRay = MapHandler.MAP.camera.getPickRay((!event.position) ? event.endPosition : event.position);
            var featuresPromise = MapHandler.MAP.imageryLayers.pickImageryLayerFeatures(pickRay, MapHandler.MAP.scene);

            if (Cesium.defined(featuresPromise)) {
                Promise.resolve(featuresPromise).then(function(features) {
                    if(features.length > 0) {
                        // Only return the first for now
                        callback(features[0]);
                    }
                });
            }
        } else {
            callback(feature);
        }
    }

}