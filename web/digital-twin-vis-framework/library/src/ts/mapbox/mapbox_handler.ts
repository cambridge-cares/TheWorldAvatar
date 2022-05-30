/**
 * Concrete implementation of the MapHandler class that handles
 * a single MapBox map instance.
 */
class MapHandler_MapBox extends MapHandler {

    /**
     * MapBox popup element.
     */
    public static POPUP;

    /**
     * Initialise and store a new map object.
     */
    public initialiseMap(mapOptions: Object) {
        // Set the mapbox api key
        // @ts-ignore
        mapboxgl.accessToken = MapHandler.MAP_API;

        // Check the options for required parameters, provide defaults if missing
        let newOptions = (mapOptions !== null) ? mapOptions : {};
        if(!newOptions["container"]) newOptions["container"] = "map"
        if(!newOptions["center"]) newOptions["center"] = [-0.1280432939529419, 51.50805967151767];
        if(!newOptions["zoom"]) newOptions["zoom"] = 16;
        if(!newOptions["style"]) newOptions["style"] = "mapbox://styles/mapbox/light-v10";

        if(MapHandler.MAP === null || MapHandler.MAP === undefined) {
            // Create new map (note the settings used here may be overriden when the map is loaded with data).
            // @ts-ignore
            MapHandler.MAP = new mapboxgl.Map(newOptions);

            // Setup mouse interactions
            MapHandler.MAP.on("click", (event) => this.handleClick(event));
            MapHandler.MAP.on("mousemove", (event) => this.handleMouse(event));

            // Create popup
            MapHandler_MapBox.POPUP = new mapboxgl.Popup({
                closeButton: false,
                closeOnClick: false,
                maxWidth: "400px"
            });
        }  else {
            // Reinitialise state of existing map
            MapHandler.MAP.setStyle(newOptions["style"]);
            delete newOptions["style"];
            delete newOptions["container"];

            MapHandler.MAP.jumpTo(newOptions);
        }

        // Store terrain URL
        if(mapOptions["style"].includes("light")) window.terrain = "light";
        if(mapOptions["style"].includes("dark")) window.terrain = "dark";
        if(mapOptions["style"].includes("outdoors")) window.terrain = "outdoors";
        if(mapOptions["style"].includes("satellite")) window.terrain = "satellite";
    }

    /**
     * Handles a click event on the map.
     * 
     * @param event mouse event
     */
    private handleClick(event) {
        if(!MapHandler.ALLOW_CLICKS) return;

        // Get all visible features under the mouse click
        let features = MapHandler.MAP.queryRenderedFeatures(event.point);

        // Filter out non-CMCL layers
        features = features.filter(feature => {
            return MapBoxUtils.isCMCLLayer(feature);
        });

        if(features.length > 1) {
            // Click on overlapping, individual features/clusters
            this.clickMultiple(features);

        } else if (features.length === 1) {

            // Click on single feature (or single cluster)
            let layer = Manager.CURRENT_GROUP.getLayerWithID(features[0]["layer"]["id"]);
            layer.handleClick(features[0]);
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
        await MapBoxUtils.recurseFeatureNames(leafs, features);

        // TODO - Use leafs variable to build drop-down tree.
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

        } else if(features.length === 1) {
            // Mouse over single feature
            MapHandler.MAP.getCanvas().style.cursor = 'pointer';

            let feature = features[0];
            let layer = Manager.CURRENT_GROUP.getLayerWithID(feature["layer"]["id"]);

            if(layer != null && layer instanceof MapBoxLayer) {
                (<MapBoxLayer> layer).handleMouseEnter(feature);
            }

        } else {
            // Mouse over multiple features
            MapHandler.MAP.getCanvas().style.cursor = 'pointer';
        }
    }

    /**
     * OVERRIDE: Plot the contents of the input data group on the map.
     */
    public plotData(dataStore: DataStore) {
        dataStore.dataGroups.forEach(rootGroup => {
            console.log("Plotting root group?");
            let allLayers = rootGroup.flattenDown();
            allLayers.forEach(layer => {
                this.plotLayer(rootGroup, layer);
            });
        });
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
        // @ts-ignore
        let collision = MapHandler.MAP.getSource(source.id);

        if(collision === null || collision === undefined) {
            // Clone the original source definition
            let options = {...source.definition};

            // Remove properties not expected by MapBox
            if(options["metaFiles"]) delete options["metaFiles"];
            if(options["timeseriesFiles"]) delete options["timeseriesFiles"];

            // Add attributions if missing
            if(source.type !== "video" && source.type !== "image") {
                if(!options["attribution"]) {
                    options["attribution"] = "CMCL Innovations";
                }
            }

            // Add to the map
            MapHandler.MAP.addSource(source.id, options);
            console.info("Added source to MapBox map: " + source.id);
        }
    }

    /**
     * Adds the data layer to the map.
     * 
     * @param layer layer to add.
     */
    private addLayer(layer: DataLayer) {
        let collision = MapHandler.MAP.getLayer(layer.id);

        if(collision === null || collision === undefined) {
            // Clone the original layer definition
            let options = {...layer.definition};

            // Add attributions if missing
            if(!options["metadata"]) {
                options["metadata"] = {};
            }
            if(!options["metadata"]["attribution"]) {
                options["metadata"]["attribution"] = "CMCL Innovations";
            }

            // Remove 'clickable' if specified
            if(options["clickable"]) {
                options["metadata"]["clickable"] = options["clickable"]
                delete options["clickable"]
            } else {
                options["metadata"]["clickable"] = true
            }

            // Update to unique ID
            options["id"] = layer.id;

            // Remove fields not required by MapBox
            delete options["name"];

            // Add to the map
            MapHandler.MAP.addLayer(options);
            console.info("Added layer to MapBox map '" + layer.id + "'.");
        }
    }

    /**
     * 
     */
    private async handleIcons(rootGroup: DataGroup) {
        // let iconFile = rootGroup.location + "/icons.json";

        // // Load the JSON
        // let json = await $.getJSON(iconFile, function(json) {
        //     return json;
        // }).fail((error) => {
        //     console.warn("Could not read 'icons.json' file; it's an optional file so skipping...")
        // });
        
        // // Load images once JSON has loaded
        // var promises = [];
        // for(var key of Object.keys(json)) {

        //     // Create a promise that resolves once the image is loaded AND added
        //     let promise = new Promise((resolve, reject) => {
        //         let imageName = key;
        //         let imageFile = rootGroup.location + "/" + json[key];

        //         let hasImage = MapHandler.MAP.hasImage(imageName);
        //         if(!hasImage) {

        //             MapHandler.MAP.loadImage(
        //                 imageFile,
        //                 (error, image) => {
        //                     if(error) {
        //                         console.log(error);
        //                         reject(error);
        //                     }

        //                     MapHandler.MAP.addImage(imageName, image);
        //                     resolve([]);
        //                 }
        //             );
        //         }
        //     });
        //     promises.push(promise);
        // }

        // return Promise.all(promises);
    }

}