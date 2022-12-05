/**
 * Concrete implementation of the MapHandler class that handles
 * a single CesiumJS map instance.
 */
class MapHandler_Cesium extends MapHandler {

    /**
     * Mapbox popup element.
     */
    public static POPUP;

    /**
     * Map of data source keyed by layer name.
     */
    public static DATA_SOURCES = {};

    /**
     * Constructor.
     */
    constructor(manager: Manager) {
        super(manager);
    }

    /**
     * Initialise and store a new map object.
     */
    public initialiseMap(mapOptions: Object) {

        MapHandler.MAP_OPTIONS = mapOptions;

        if(MapHandler.MAP === null || MapHandler.MAP === undefined) {

            // Initialize the Cesium Viewer in the HTML element with the `cesiumContainer` ID.
            MapHandler.MAP = new Cesium.Viewer('map', {
                timeline: false,
                animation: false,
                baseLayerPicker: false, 
                homeButton: false, 
                infoBox: false, 
                navigationHelpButton: false,
                projectionPicker: false,
                fullscreenButton: false,
                geocoder: false,
                selectionIndicator: false,
                sceneModePicker: false
            }); 

            // Set the underlying globe color
            MapHandler.MAP.scene.globe.undergroundColor = Cesium.Color.GREY;

            // Set the globe translucency (if provided)
            if(mapOptions.hasOwnProperty("opacity")) {
                MapHandler.MAP.scene.globe.translucency.enabled = true;

                let value = mapOptions["opacity"];
                MapHandler.MAP.scene.globe.translucency.frontFaceAlphaByDistance = 
                        new Cesium.NearFarScalar(1000.0, Math.abs(value), 2000.0, 1.0);
            }

            // Include terrain in Z-index rendering (otherwise we'll be able to see other entities through it).
            MapHandler.MAP.scene.globe.depthTestAgainstTerrain = true

            // Build the URL to pull tile imagery from Mapbox (defaults to dark theme)
            var tileURL = getDefaultImagery();
            if(tileURL.endsWith("access_token=")) {
                tileURL = tileURL + MapHandler.MAP_API;
            }

            // Remove any existing imagery providers and add our own
            MapHandler.MAP.imageryLayers.removeAll(true);
            let imageryProvider = new Cesium.UrlTemplateImageryProvider({
                url: tileURL,
                credit: "mapbox"
            });
            MapHandler.MAP.scene.imageryLayers.addImageryProvider(imageryProvider);

            // Override mouse controls 
            let controller = MapHandler.MAP.scene.screenSpaceCameraController;
            controller.tiltEventTypes = [Cesium.CameraEventType.RIGHT_DRAG];
            controller.zoomEventTypes = controller.zoomEventTypes.filter(item => item !== Cesium.CameraEventType.RIGHT_DRAG);

            // Dodgy, but the only way to change the zoom increment
            controller._zoomFactor = 2;

            // Enable picking
            let handler = new Cesium.ScreenSpaceEventHandler(MapHandler.MAP.scene.canvas);
            handler.setInputAction(event => this.handleClick(event), Cesium.ScreenSpaceEventType.LEFT_CLICK);

            // Enable hover-over silhouette
            CesiumUtils.enableSilhouettes();
            handler.setInputAction(event => this.handleMouse(event), Cesium.ScreenSpaceEventType.MOUSE_MOVE);

            MapHandler.MAP.camera.setView({
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["center"][0], mapOptions["center"][1], mapOptions["center"][2]),
                orientation: {
                    heading: Cesium.Math.toRadians(mapOptions["heading"]),
                    pitch: Cesium.Math.toRadians(mapOptions["pitch"]),
                    roll: Cesium.Math.toRadians(mapOptions["roll"])
                }
            });
            MapHandler.MAP.scene.requestRender();

            // Setup keyboard shortcuts
            CesiumUtils.setupKeyboardShortcuts();

            // Enable terrain elevations (if set)
            this.addTerrain();

        } else {
            MapHandler.MAP.camera.setView({
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["center"][0], mapOptions["center"][1], mapOptions["center"][2]),
                orientation: {
                    heading: Cesium.Math.toRadians(mapOptions["heading"]),
                    pitch: Cesium.Math.toRadians(mapOptions["pitch"]),
                    roll: Cesium.Math.toRadians(mapOptions["roll"])
                }
            });
            MapHandler.MAP.scene.requestRender();
        }
    }

    /**
     * Creates and adds a CesiumTerrainProvider based on the "terrain" object
     * found in the settings.json file (if present).
     */
    public addTerrain() {
        let terrainOptions = Manager.SETTINGS.getSetting("terrain");
        if(terrainOptions === null || terrainOptions === undefined) return;

        // Create provider and add to map
        let terrainProvider = new Cesium.CesiumTerrainProvider(terrainOptions);
        MapHandler.MAP.terrainProvider = terrainProvider;
    }

    /**
     * Handles a click event on the map.
     * 
     * @param event mouse event
     */
    public handleClick(event) {
        if(!MapHandler.ALLOW_CLICKS) return;

        // Get the feature at the click point
        let self = this;
        CesiumUtils.getFeature(event, function(feature) {
            window.currentFeature = feature;

            if(feature instanceof Cesium.ImageryLayerFeatureInfo) {
                // 2D WMS feature
                let properties = {...feature.data.properties};
                self.manager.showFeature(feature, properties);
            } else {
                // 3D feature
                let properties = {};
                let contentMetadata = feature?.content?.metadata;
    
                // Transform properties for compatability with manager code
                if (Cesium.defined(contentMetadata)) {
                    properties = {...contentMetadata["_properties"]};

                } else if(typeof feature.getPropertyIds === "function") {
                    // No metadata refined, try to get properties via id
                    let ids = feature.getPropertyIds();
                    if(ids != null) {
                        ids.forEach(id => {
                            properties[id] = feature.getProperty(id);
                        });
                    }
                } else {
                    // Unknown data type
                    return;
                }
                
                self.manager.showFeature(feature, properties);
                CesiumUtils.setSelectedSilhouette(feature, event);
            }
        });
    }

    /**
     * Handles logic when the mouse cursor moves over or out of features.
     * 
     * @param event mouse event
     */
    private handleMouse(event) {
        if(!MapHandler.ALLOW_CLICKS) return;
        let metaBox = document.getElementById("cesiumMetaBox");
        metaBox.style.display = "none";

        // Get the feature at the click point
        CesiumUtils.getFeature(event, function(feature) {

            if(feature instanceof Cesium.ImageryLayerFeatureInfo) {
                // 2D WMS feature
                let properties = {...feature.data.properties};
                let name = getName(properties);

                if(name != null && name !== "") {
                    metaBox.style.display = "block";
                    metaBox.style.bottom = `${MapHandler.MAP.canvas.clientHeight - event.endPosition.y + 50}px`;
                    metaBox.style.left = `${event.endPosition.x - 100}px`;
                    metaBox.innerHTML = CesiumUtils.getPopupContent(properties);
                } 

            } else {
                // 3D feature
                let properties = {};
                let contentMetadata = feature?.content?.metadata;
    
                // Transform properties for compatability with manager code
                if (Cesium.defined(contentMetadata)) {
                    properties = {...contentMetadata["_properties"]};
                } else {
                    // Do nothing, there's no data?
                }

                let name = getName(properties);
                if(name != null && name !== "") {
                    metaBox.style.display = "block";
                    metaBox.style.bottom = `${MapHandler.MAP.canvas.clientHeight - event.endPosition.y + 50}px`;
                    metaBox.style.left = `${event.endPosition.x - 100}px`;
                    metaBox.innerHTML = CesiumUtils.getPopupContent(properties);
                }
            }
        });
    }

    /**
     * Plot the contents of the input data group on the map.
     */
    public plotData(dataStore: DataStore) {
        // Get all layers from all groups
        let allLayers = [];
        dataStore.dataGroups.forEach(rootGroup => {
            let groupLayers = rootGroup.flattenDown();
            allLayers = allLayers.concat(groupLayers);
        });

        // Collect the WMS layers
        let wmsLayers = allLayers.filter(layer => {
            return layer.source.type === "wms" || layer.source.type === "geoserver"; 
        });

        // Order them
        wmsLayers = wmsLayers.sort((a, b) => {
            if(a.order > b.order) return 1;
            if(a.order < b.order) return -1;
            return 0;
        });

        // Plot them
        wmsLayers.forEach(layer => this.plotLayer(null, layer));

        // Collect the non-WMS layers and plot them
        let otherLayers = allLayers.filter(layer => {
            return layer.source.type !== "wms" && layer.source.type !== "geoserver"; 
        });
        otherLayers.forEach(layer => this.plotLayer(null, layer));
    }

    /**
     * Creates a visual layer on the map based on the input layer definition.
     * 
     * @param group DataGroup containing the layer.
     * @param layer definition of layer to create.
     */
    public plotLayer(group: DataGroup, layer: DataLayer) {
        let source = layer.source;
        if(source === null || source === undefined) return;

        // Add the layer's source to the map
        this.addSource(source);

        // Add the layer itself to the map
        this.addLayer(layer);
    }

    /**
     * Adds the data source to the map.
     * 
     * @param group group containing the source.
     * @param source data source.
     */
    private addSource(source: DataSource) {
       // Not used in Cesium implementation
    }

    /**
     * Adds the data layer to the map.
     * 
     * @param layer layer to add.
     */
    private addLayer(layer: DataLayer) {
        let source = layer.source.definition;

        // Check the required parameters for ALL types
        let dataURI = source["uri"];
        if(dataURI === null || dataURI === undefined) {
            console.error("Cannot plot a data source that has no 'uri' parameter");
        }

        switch(source["type"].toLowerCase()) {
            // 2D data from geoserver
            case "wms":
            case "geoserver": {
                this.addWMS(source, layer);
            }
            break;

            // Individual, non-tiled, KML files
            case "kml": {
                this.addKMLFile(source, layer);
            }
            break;

            // Individual, non-tiled, glTF/glB files
            case "glb":
            case "gltf": {
                this.addGLTFFile(source, layer);
            }
            break;

            // 3D tiles
            case "tile":
            case "tiles": {
                this.addTileset(source, layer);
            }
            break;

            // Anything else
            default: {
                console.warn("Unknown type '" + source["type"] + "', skipping this data source.");
            }
            break;
        }
    }

    /**
     * Adds an individual KML file to the map.
     * 
     * @param source JSON definition of source data.
     * @param layerID ID of layer upon the map.
     */
    private addKMLFile(source: Object, layer: DataLayer) {
        let sourceKML = Cesium.KmlDataSource.load(source["uri"]);
        sourceKML.then((result) => {
            
            result["show"] = layer.definition["visibility"] == undefined || layer.definition["visibility"] === "visible";
            result["layerID"] = layer.id;

            MapHandler.MAP.dataSources.add(result);
            console.info("Added KML source to map with layer ID: "+ layer.id);
    
            // Cache knowledge of this source, keyed by layer id
            if(MapHandler_Cesium.DATA_SOURCES[layer.id] === null || MapHandler_Cesium.DATA_SOURCES[layer.id] === undefined) {
                MapHandler_Cesium.DATA_SOURCES[layer.id] = [];
            }
            MapHandler_Cesium.DATA_SOURCES[layer.id].push(result);
        });
    }

    /**
     * Adds an individual, non-tiled, glTF/glB file to the map.
     * 
     * @param source JSON definition of source data. 
     * @param layerID ID of layer upon the map.
     */
    private addGLTFFile(source: Object,  layer: DataLayer) {
        // Check the position
        let position = source["position"];
        if(position === null || position === undefined) {
            console.error("Cannot plot a glTF/glB data source that has no 'position' parameter");
        }

        // Check the orientation
        let orientation = [0, 0, 0];
        if(source.hasOwnProperty("orientation")) {
            orientation = source["orientation"];
        }

        // Generate final position
        let finalPosition = Cesium.Cartesian3.fromDegrees(position[0], position[1])

        // Generate final orientation
        let finalOrientation = Cesium.Transforms.headingPitchRollQuaternion(
            finalPosition,
            new Cesium.HeadingPitchRoll(orientation[0], orientation[1], orientation[2])
        );

        // Define the entity before adding to the map
        let sourceEntity = {
            position: finalPosition,
            orientation: finalOrientation,
            model: {
                uri: source["uri"],
                scale: source.hasOwnProperty("scale") ? source["scale"] : 1.0
            },
            show: layer.definition["visibility"] == undefined || layer.definition["visibility"] === "visible"
        };

        MapHandler.MAP.entities.add(sourceEntity);
        console.info("Added glTF/glB source to map with layer ID: "+ layer.id);

        // Cache knowledge of this source, keyed by layer id
        if(MapHandler_Cesium.DATA_SOURCES[layer.id] === null || MapHandler_Cesium.DATA_SOURCES[layer.id] === undefined) {
            MapHandler_Cesium.DATA_SOURCES[layer.id] = [];
        }
        MapHandler_Cesium.DATA_SOURCES[layer.id].push(sourceEntity);
    }

    /**
     * Adds a 3D tileset to the map.
     * 
     * @param source JSON definition of source data. 
     * @param layerID ID of layer upon the map.
     */
    private addTileset(source: Object, layer: DataLayer) {
        // Check the position (if set)
        let position = source["position"];
        if(position !== null && position !== undefined) {
            let centerCartesian = Cesium.Cartesian3.fromDegrees(position[0], position[1], position[2]);
            position = Cesium.Transforms.eastNorthUpToFixedFrame(centerCartesian);
        }

        // Check the rotation (if set)
        let rotation = source["rotation"];
        if(rotation != null && rotation !== undefined && position != null) {
            // Create a heading-pitch-roll object
            let hpr = new Cesium.HeadingPitchRoll(rotation[2], rotation[1], rotation[0]);

            // Create a rotation matrix
            let rotationMatrix = Cesium.Matrix3.fromHeadingPitchRoll(hpr, new Cesium.Matrix3());

            // And multiply the model matrix (position) by this rotation matrix
            Cesium.Matrix4.multiplyByMatrix3(position, rotationMatrix, position);
        }

        // Define tileset options
        let options = {
            url: source["uri"],
            show: layer.definition["visibility"] == undefined || layer.definition["visibility"] === "visible"
        };

        if(position !== null && position !== undefined) {
            options["modelMatrix"] = position;
        }

        // If clipping is enabled, add a clipping plane
        let clippingPlanes = null;

        if(layer.definition.hasOwnProperty("clipPlane") && layer.definition["clipPlane"]) {
            clippingPlanes = new Cesium.ClippingPlaneCollection({
                planes: [
                    new Cesium.ClippingPlane(new Cesium.Cartesian3(0.0, 0.0, -1.0), 0.0)
                ],
                edgeWidth: 1.0
            });
            options["clippingPlanes"] = clippingPlanes;
        }

        // Create tileset object
        let tileset = new Cesium.Cesium3DTileset(options);
        tileset["layerID"] = layer.id;

        // Setup clipping plane logic
        if(clippingPlanes != null) CesiumUtils.prepareClippingPlane(tileset, clippingPlanes);

        // Add the tileset to the map
        MapHandler.MAP.scene.primitives.add(tileset);
        console.info("Added 3D tileset source to map with layer ID: "+ layer.id);
         
        // Cache knowledge of this source, keyed by layer id
        if(MapHandler_Cesium.DATA_SOURCES[layer.id] === null || MapHandler_Cesium.DATA_SOURCES[layer.id] === undefined) {
            MapHandler_Cesium.DATA_SOURCES[layer.id] = [];
        }
        MapHandler_Cesium.DATA_SOURCES[layer.id].push(tileset);
    }

    /**
     * Add a WMS imagery layer from geoserver.
     * 
     * @param url 
     * @param layerID 
     */
    private addWMS(source: Object, layer: DataLayer) {
        // Check the geoserver layer name
        let wmsLayer = source["wmsLayer"];
        if(wmsLayer === null || wmsLayer === undefined) {
            console.error("Cannot plot a WMS data source that has no 'wmsLayer' parameter");
        }

        let provider = new Cesium.WebMapServiceImageryProvider({
            url: source["uri"],
            layers: wmsLayer,
            parameters: {
                transparent: source.hasOwnProperty("transparency") ? source["transparency"] : false,
                format: source.hasOwnProperty("format") ? source["format"] : "image/png"
            },
            credit: layer.id,
        });
        provider["layerID"] = layer.id;

        let layers = MapHandler.MAP.imageryLayers;
        layers.addImageryProvider(provider);

        // Now that it's added, we can hide it (unfortunatly there's no constructor option for this)
        for(let i = 0; i < layers.length; i++) {
            if(layers.get(i).imageryProvider === provider) {
                layers.get(i).show = layer.definition["visibility"] == undefined || layer.definition["visibility"] === "visible"
            }
        }

        // Cache knowledge of this source, keyed by layer id
        if(MapHandler_Cesium.DATA_SOURCES[layer.id] === null || MapHandler_Cesium.DATA_SOURCES[layer.id] === undefined) {
            MapHandler_Cesium.DATA_SOURCES[layer.id] = [];
        }
        MapHandler_Cesium.DATA_SOURCES[layer.id].push(provider);
    }

}