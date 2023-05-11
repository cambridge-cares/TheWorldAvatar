 /**
 * Utilities specific to Mapbox implementations
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
	 * Show or hide a single (Mapbox) layer on the map.
	 * 
	 * @param {String} layerID Mapbox layer name.
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
     * Change the underlying Mapbox style.
     * 
     * @param {String} mode 
     */
    public static changeTerrain(mode) {
        let imagerySettings = Manager.SETTINGS.getSetting("imagery");
        if(imagerySettings == null) return;

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

        let tileURL = imagerySettings[mode] as string;
        if(tileURL == null) return;

        // Add the API if missing
        if(tileURL.endsWith("access_token=")) {
            tileURL += MapHandler.MAP_API;
        }

        // Add our own imagery provider
        let imageryProvider = new Cesium.UrlTemplateImageryProvider({
            url: tileURL,
            credit: "mapbox"
        });
        MapHandler.MAP.scene.imageryLayers.addImageryProvider(imageryProvider, 0);
        MapHandler.MAP.scene.requestRender();
    }

    /**
     * Generates a JSON object defining the default imagery options if none is provided
     * by the developer in the settings.json file.
     */
    public static generateDefaultImagery() {
        let imagerySettings = {};

        // Add possible imagery options
        imagerySettings["Light"] =
            "https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/256/{z}/{x}/{y}?access_token="
            + MapHandler.MAP_API;
        imagerySettings["Dark"] =
            "https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/256/{z}/{x}/{y}?access_token="
            + MapHandler.MAP_API;
        imagerySettings["Outdoors"] =
            "https://api.mapbox.com/styles/v1/mapbox/outdoors-v11/tiles/256/{z}/{x}/{y}?access_token="
            + MapHandler.MAP_API;
        imagerySettings["Satellite (Raw)"] =
            "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/256/{z}/{x}/{y}?access_token="
            + MapHandler.MAP_API;
        imagerySettings["Satellite (Labelled)"] =
            "https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v11/tiles/256/{z}/{x}/{y}?access_token="
            + MapHandler.MAP_API;

        // Set default imagery to Dark
        imagerySettings["default"] = "Dark";

        // Push settings
        Manager.SETTINGS.putSetting("imagery", imagerySettings);
    }

    /**
     * Reset the camera to default position.
     */
    public static resetCamera() {
        let mapOptions = MapHandler.MAP_OPTIONS;
        if(mapOptions == null) return;

        MapHandler.MAP.camera.flyTo({
            destination : Cesium.Cartesian3.fromDegrees(mapOptions["center"][0], mapOptions["center"][1], mapOptions["center"][2]),
            orientation: {
                heading: Cesium.Math.toRadians(mapOptions["heading"]),
                pitch: Cesium.Math.toRadians(mapOptions["pitch"]),
                roll: Cesium.Math.toRadians(mapOptions["roll"])
            }
        });
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
     * 
     */
    public static clearSilhouette() {
        CesiumUtils.OUTLINE_GREEN.selected = [];
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

    /**
     * Generates content for pop-up box displayed on hover over.
     */
    public static getPopupContent(properties: Object) {
        // Get feature details
        let name = getName(properties);
        let desc = getDescription(properties);

        // Make HTML string
        let html = "";
        if(desc == null) {
            html += "<h3 style='text-align: center !important;'>" + name + "</h3>";
        } else {
            html += "<h3>" + name + "</h3>";
            if(desc.length > 100) {
                html += "<div class='desc-popup long-popup'></br>" + desc + "</div>";
            } else {
                html += "<div class='desc-popup'></br>" + desc + "</div>";
            }
        }

        // Add thumbnail if present
        if(properties["thumbnail"]) {
            html += "<br/><br/>";
            html += "<img class='thumbnail' src='" + properties["thumbnail"] + "'>";
        }
        return html;
    }

    /**
     * Sets up keyboard shortcuts for the Cesium camera. This has been added so that
     * users without a mouse (or with a non-standard touchpad) can still control the
     * camera.
     */
    public static setupKeyboardShortcuts() {
        // TODO: Add full camera controls here. This will be tricky as Cesium offers
        // no programmatic way to pan the camera around 2D plane (mimicking the mouse
        // movement logic).
        window.addEventListener("keydown", function (event) {
            if (event.defaultPrevented) return;
          
            switch (event.key) {
                case "PageUp":
                    MapHandler.MAP.camera.zoomIn(25);
                    break;
                case "PageDown":
                    MapHandler.MAP.camera.zoomOut(25);
                    break;
                default:
                    return;
            }
          
            // Cancel the default action to avoid it being handled twice
            event.preventDefault();
        }, true);
    }

    /**
     * Flys the camera to the input feature, can be unreliable.
     * 
     * @param feature 
     */
    public static flyToFeature(feature: Object) {
        // Is this a WMS feature?
        if(feature instanceof Cesium.ImageryLayerFeatureInfo) {
            const position = MapHandler.MAP.scene.globe.ellipsoid.cartographicToCartesian(
                // @ts-ignore
                feature.position
            );

            let offset = CesiumUtils.offsetFromHeadingPitchRange(
                MapHandler.MAP.camera.heading,
                MapHandler.MAP.camera.pitch,
                50
            );
            const transform = Cesium.Transforms.eastNorthUpToFixedFrame(position);
            Cesium.Matrix4.multiplyByPoint(transform, offset, position);
            MapHandler.MAP.camera.flyTo({
                destination: position,
                orientation: {
                    heading: MapHandler.MAP.camera.heading,
                    pitch: MapHandler.MAP.camera.pitch,
                },
                easingFunction: Cesium.EasingFunction.QUADRATIC_OUT,
            });
            return;
        }

        if(!feature.hasOwnProperty("Longitude")) {
            // Cesium has not stored position of this feature, cannot fly to
            return;
        }

        // 3D feature
        const positionCartographic = new Cesium.Cartographic(
            // @ts-ignore
            Cesium.Math.toRadians(feature.getProperty("Longitude")),
            // @ts-ignore
            Cesium.Math.toRadians(feature.getProperty("Latitude")),
            // @ts-ignore
            feature.getProperty("Height") * 0.5
        );
        const position = MapHandler.MAP.scene.globe.ellipsoid.cartographicToCartesian(
            positionCartographic
        );

        let offset = CesiumUtils.offsetFromHeadingPitchRange(
            MapHandler.MAP.camera.heading,
            MapHandler.MAP.camera.pitch,
            // @ts-ignore
            feature.getProperty("Height") * 2.0
        );

        const transform = Cesium.Transforms.eastNorthUpToFixedFrame(position);
        Cesium.Matrix4.multiplyByPoint(transform, offset, position);

        MapHandler.MAP.camera.flyTo({
            destination: position,
            orientation: {
                heading: MapHandler.MAP.camera.heading,
                pitch: MapHandler.MAP.camera.pitch,
            },
            easingFunction: Cesium.EasingFunction.QUADRATIC_OUT,
        });
    }

    /**
     * https://sandcastle.cesium.com/?src=3D%20Tiles%20Interactivity.html
     * 
     * @param heading 
     * @param pitch 
     * @param range 
     * @returns 
     */
    public static offsetFromHeadingPitchRange(heading, pitch, range) {
        pitch = Cesium.Math.clamp(
          pitch,
          -Cesium.Math.PI_OVER_TWO,
          Cesium.Math.PI_OVER_TWO
        );
        heading = Cesium.Math.zeroToTwoPi(heading) - Cesium.Math.PI_OVER_TWO;
      
        const pitchQuat = Cesium.Quaternion.fromAxisAngle(
          Cesium.Cartesian3.UNIT_Y,
          -pitch
        );
        const headingQuat = Cesium.Quaternion.fromAxisAngle(
          Cesium.Cartesian3.UNIT_Z,
          -heading
        );
        const rotQuat = Cesium.Quaternion.multiply(
          headingQuat,
          pitchQuat,
          headingQuat
        );
        const rotMatrix = Cesium.Matrix3.fromQuaternion(rotQuat);
      
        const offset = Cesium.Cartesian3.clone(Cesium.Cartesian3.UNIT_X);
        Cesium.Matrix3.multiplyByVector(rotMatrix, offset, offset);
        Cesium.Cartesian3.negate(offset, offset);
        Cesium.Cartesian3.multiplyByScalar(offset, range, offset);
        return offset;
    }
}