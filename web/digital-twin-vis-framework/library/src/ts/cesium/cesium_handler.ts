/**
 * Concrete implementation of the MapHandler class that handles
 * a single MapBox map instance.
 */
class MapHandler_Cesium extends MapHandler {

    /**
     * MapBox popup element.
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
        if(MapHandler.MAP === null || MapHandler.MAP === undefined) {
            // Initialize the Cesium Viewer in the HTML element with the `cesiumContainer` ID.
            // @ts-ignore
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

                // @ts-ignore
                imageryProvider: new Cesium.MapboxStyleImageryProvider({
                    styleId: mapOptions["style"],
                    accessToken: MapHandler.MAP_API
                })
            }); 

            // Override default zoom level
            console.log("---");
            console.log(MapHandler.MAP.scene.screenSpaceCameraController);
            console.log("---");
            
            let controller = MapHandler.MAP.scene.screenSpaceCameraController;
            // @ts-ignore
            controller.tiltEventTypes = [Cesium.CameraEventType.RIGHT_DRAG];
            // @ts-ignore
            controller.zoomEventTypes = controller.zoomEventTypes.filter(item => item !== Cesium.CameraEventType.RIGHT_DRAG);

            console.log("---");
            console.log(MapHandler.MAP.scene.screenSpaceCameraController);
            console.log("---");

            MapHandler.MAP.camera.setView({
                // @ts-ignore
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    // @ts-ignore
                    heading: Cesium.Math.toRadians(mapOptions["heading"]),
                    // @ts-ignore
                    pitch: Cesium.Math.toRadians(mapOptions["pitch"]),
                    // @ts-ignore
                    roll: Cesium.Math.toRadians(mapOptions["roll"])
                }
            });

        }  else {
            MapHandler.MAP.camera.setView({
                // @ts-ignore
                destination : Cesium.Cartesian3.fromDegrees(mapOptions["target"][0], mapOptions["target"][1], mapOptions["target"][2]),
                orientation: {
                    // @ts-ignore
                    heading: Cesium.Math.toRadians(mapOptions["heading"]),
                    // @ts-ignore
                    pitch: Cesium.Math.toRadians(mapOptions["pitch"]),
                    // @ts-ignore
                    roll: Cesium.Math.toRadians(mapOptions["roll"])
                }
            });
        }
    }

    /**
     * Handles a click event on the map.
     * 
     * @param event mouse event
     */
    public handleClick(event, feature) {
        if(!MapHandler.ALLOW_CLICKS) return;

        // Get all visible features under the mouse click
        let features = [];
        if(feature !== null && feature !== undefined) {
            features.push(feature);
        } else {
            features = MapHandler.MAP.queryRenderedFeatures(event.point);
        }

        // Filter out non-CMCL layers
        features = features.filter(feature => {
            return MapBoxUtils.isCMCLLayer(feature);
        });

        // Filter out duplicates (MapBox can return these if a feature is split across a tile boundary)
        features = MapBoxUtils.deduplicate(features);

        if(features.length > 1) {
            // Click on overlapping, individual features/clusters
            this.clickMultiple(features);

        } else if (features.length === 1) {
            let feature = features[0];

            let layer = Manager.DATA_STORE.getLayerWithID(feature["layer"]["id"]);
            let clickable = layer.definition["clickable"];
            if(clickable !== null && clickable === false) {
                // No mouse interaction
                return;
            }

            if(MapBoxUtils.isCluster(feature)) {
                // Clicked on a clustered feature, handle as if multiple
                this.clickMultiple(features);

            } else {
                // Click on single feature
                console.log(feature);
                this.manager.showFeature(feature);
            }
        }
    }

    /**
     * Triggered when the user clicks on the map and multiple features are underneath
     * (these could be overlapping individual features or cluster points).
     * 
     * @param features list of features/clusters under mouse
     */
    private async clickMultiple(features: Array<Object>) {
        let leafs = [];
        await MapBoxUtils.recurseFeatures(leafs, features);

        // Cache features offered by the select box
        window.selectFeatures = {};

        // Sort the leafs by layer name
        let sortedLeafs = {};

        // Group the features by layer
        for(let i = 0; i < leafs.length; i++) {
            let leaf = leafs[i];
            let layerID = leaf["layer"]["id"];
            let layer = Manager.DATA_STORE.getLayerWithID(layerID);

            let clickable = layer.definition["clickable"];
            if(clickable !== null && clickable === false) {
                // No mouse interaction
                continue;
            }

            if(sortedLeafs[layer.name] === null || sortedLeafs[layer.name] === undefined) {
                sortedLeafs[layer.name] = [];
            }
            sortedLeafs[layer.name].push(leaf);
        }

        // Build drop down
        let html = `
            <p>
                Multiple closely spaced features are located at these coordinates. 
                <br/><br/>
                Please choose which individual feature you'd like to select using the drop-down box below. 
                Features are grouped by their containing layer.
            </p>
            </br>
            <div id="featureSelectContainer">
                <select name="features" id="featureSelect" onchange="manager.featureSelectChange(this)">
                    <option value="" disabled selected>Select a feature from a layer...</option>
        `;

        // Add option header for each layer
        for(const [key, value] of Object.entries(sortedLeafs)) {
            let leafs = value as Array<Object>;
            html += "<optgroup label='" + key + "'>";

            // Add option for each feature
            leafs.forEach(leaf => {
                let featureName = (leaf["properties"]["name"] !== null && leaf["properties"]["name"] !== undefined) ? leaf["properties"]["name"] : "Feature #" + leaf["id"];
                let layerID = leaf["layer"]["id"];
                let value = leaf["id"] + "@" + layerID;

                let optionHTML = `
                    <option value="` + value + `">` + 
                    featureName +
                    `</option>
                `;
                html += optionHTML;

                // Cache feature
                window.selectFeatures[value] = leaf;
            });
            html += "</optgroup>";
        }

        // Close HTML
        html += `
                </select>
            </div>
        `;

        // Update the side panel
        document.getElementById("titleContainer").innerHTML = "<h2>Multiple locations...</h2>";
        document.getElementById("contentContainer").innerHTML = html;
    }

    /**
     * Handles logic when the mouse cursor moves over or out of features.
     * 
     * @param event mouse event
     */
    private handleMouse(event) {
        // Get a list of features under the mouse
        let features = MapHandler.MAP.queryRenderedFeatures(event.point);
        features = features.filter(feature => {
            return MapBoxUtils.isCMCLLayer(feature);
        });

        if(features.length === 0) {
            // Mouse no longer over any features
            MapHandler.MAP.getCanvas().style.cursor = '';
            if(MapHandler_MapBox.POPUP !== null) MapHandler_MapBox.POPUP.remove();

        } else if(features.length > 0) {
            // Mouse over single feature
            let feature = features[0];
            let layer = Manager.DATA_STORE.getLayerWithID(feature["layer"]["id"]);

            let clickable = layer.definition["clickable"];
            if(clickable !== null && clickable === false) {
                // No mouse interaction
                return;
            }

            // Change cursor
            MapHandler.MAP.getCanvas().style.cursor = 'pointer';

            if(layer != null && layer instanceof MapBoxLayer) {
                if(feature !== null) MapBoxUtils.showPopup(event, feature);
            } 
        } 
    }

    /**
     * Plot the contents of the input data group on the map.
     */
    public plotData(dataStore: DataStore) {
        dataStore.dataGroups.forEach(rootGroup => {
            let allLayers = rootGroup.flattenDown();
            
            allLayers.forEach(layer => {
                this.plotLayer(rootGroup, layer);
            });
        });
        console.log("Finished adding data to map");
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
       // No used in Cesium implementation
    }

    /**
     * Adds the data layer to the map.
     * 
     * @param layer layer to add.
     */
    private addLayer(layer: DataLayer) {
        // TODO - Check for a collision


        let source = layer.source;

        switch(source.type.toLowerCase()) {

            // Individual KML files
            case "kml": {
                let locations = source.definition["data"];

                if(Array.isArray(locations)) {
                    locations.forEach(location => {
                        this.addKMLFile(location, layer.id);
                    });
                } else {
                    let kmlSource = this.addKMLFile(locations, layer.id);
                    MapHandler.MAP.zoomTo(kmlSource);
                }
            }
            break;

            // 3D tiles
            case "tile":
            case "tiles": {
                let locations = source.definition["data"];
                let centers = source.definition["center"];

                if(Array.isArray(locations) && Array.isArray(centers)) {

                    for(let i = 0; i < locations.length; i++) {
                        this.addTileset(locations[i], centers[i], layer.id);
                    }
                } else {
                    this.addTileset(locations, centers, layer.id);
                }
            }
            break;


            default: {
                console.warn("Unknown source type '" + source.type + "', skipping data.");
            }
            break;
        }

    }

    /**
     * Adds an individual KML file to the map.
     * 
     * @param fileLocation location of KML file. 
     * @param layerID ID of layer upon the map.
     */
    private addKMLFile(fileLocation: string, layerID: string) {
        // @ts-ignore
        let sourceKML = Cesium.KmlDataSource.load(fileLocation);

        // TODO: Investigate if camera and canvas options are actually required here.
        MapHandler.MAP.dataSources.add(
            sourceKML,
            {
                camera: MapHandler.MAP.camera,
                canvas: MapHandler.MAP.canvas
            }
        );
        console.info("Added KML source to map with layer ID: "+ layerID);

        // Cache knowledge of this source, keyed by layer id
        if(MapHandler_Cesium.DATA_SOURCES[layerID] === null || MapHandler_Cesium.DATA_SOURCES[layerID] === undefined) {
            MapHandler_Cesium.DATA_SOURCES[layerID] = [];
        }
        MapHandler_Cesium.DATA_SOURCES[layerID].push(sourceKML);
    }

    /**
     * Adds a 3D tileset to the map.
     * 
     * @param fileLocation location of tileset JSON file. 
     * @param position x,y,z position of tileset center (in degrees).
     * @param layerID ID of layer upon the map.
     */
    private addTileset(fileLocation: string, position: number[], layerID: string) {
        // @ts-ignore
        let centerCartesian = Cesium.Cartesian3.fromDegrees(position[0], position[1], position[2]);
        // @ts-ignore
        let centerTransform = Cesium.Transforms.eastNorthUpToFixedFrame(centerCartesian);

        // @ts-ignore
        let tileset = new Cesium.Cesium3DTileset({
            url: fileLocation,
            modelMatrix: centerTransform
        });

        // Add the tileset to the map
        MapHandler.MAP.scene.primitives.add(tileset);
        console.info("Added tileset source to map with layer ID: "+ layerID);

        // Cache knowledge of this source, keyed by layer id
        if(MapHandler_Cesium.DATA_SOURCES[layerID] === null || MapHandler_Cesium.DATA_SOURCES[layerID] === undefined) {
            MapHandler_Cesium.DATA_SOURCES[layerID] = [];
        }
        MapHandler_Cesium.DATA_SOURCES[layerID].push(tileset);
    }

    /**
     * Adds icons to the map
     */
    public addIcons(iconFile: string) {
        return $.getJSON(iconFile, function(json) {
            return json;
        })
        .fail(() => {
            console.warn("Could not read icons.json, skipping.");
        })
        .done((json) => {
            if(json === null || json === undefined) return;

            let promises = [];
            let iconHandler = new IconHandler();
            for (var key of Object.keys(json)) {
                promises.push(iconHandler.loadIcon(key, json[key]));
            }

            return Promise.all(promises).then(() => {
                console.info("All images have been registered.");
            });
        });
    }
}