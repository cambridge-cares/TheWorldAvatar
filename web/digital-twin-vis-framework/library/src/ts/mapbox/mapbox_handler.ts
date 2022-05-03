/**
 * Concrete implementation of the MapHandler class that handles
 * a single MapBox map instance.
 */
class MapHandler_MapBox extends MapHandler {

    /**
     * MapBox popup element.
     */
    private popup;

    /**
     * Initialise and store a new map object.
     */
    public initialiseMap(options: Object) {
        // Set the mapbox api key
        // @ts-ignore
        mapboxgl.accessToken = MapHandler.MAP_API;

        // Check the options for required parameters, provide defaults if missing
        let newOptions = (options === null) ? {} : {...options};
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
        }  else {
            // Reinitialise state of existing map
            MapHandler.MAP.setStyle(newOptions["style"]);
            delete newOptions["style"];
            delete newOptions["container"];

            MapHandler.MAP.jumpTo(newOptions);
        }
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

        // Filter out:
        //   - generated infrastructure layers
        //   - hidden layers
        //   - default layers added by MapBox
        features = features.filter(feature => {
            let layer = feature["layer"]["id"]
            if(MapHandler.MAP.getLayoutProperty(layer, "visibility") === "none") return false;

            if(layer.includes("arrows")) return false;
            if(layer.includes("highlight")) return false;
            if(layer.includes("focus")) return false;

            if(!layer["matadata"]) {
                return false;
            } else {
                if(!layer["metadata"]["attribution"] || layer["metadata"]["attribution"] !== "CMCL Innovations") return false;
                if(!layer["metadata"]["clickable"]) return false;
            }
        });

        if(features.length == 1 && features[0]) {
            // Click on a clustered feature
        } else {
            if(features.length > 1) {
                // Click on overlapping, individual features
            } else {
                // Click on single feature
                let layer = Manager.CURRENT_GROUP.getLayerWithName(features[0]["layer"]["id"]);
                layer.handleClick(features[0]);
            }
        }
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
            return isCMCLLayer(feature);
        });

        if(features.length > 1) {
            console.log("There are " + features.length + " features under mouse");
            console.log(features);
        }

        if(features.length === 0) {
            // Mouse no longer over any features
            MapHandler.MAP.getCanvas().style.cursor = '';

        } else if(features.length === 1) {
            // Mouse over single feature
            let feature = features[0];
            let layer = Manager.CURRENT_GROUP.getLayerWithName(feature["layer"]["id"]);
            if(layer != null && layer instanceof MapBoxLayer) {
                (<MapBoxLayer> layer).handleMouseEnter(feature);
            }

        } else {
            // Mouse over multiple features
        }
    }

    /**
     * OVERRIDE: Plot the contents of the input data group on the map.
     * 
     * @param group data group to plot.
     * 
     */
    public plotGroup(group: DataGroup) {
        // Get the root group
        let rootGroup = DataUtils.getRootGroup(group);

        // Handle loading icons that may be required by the layers
        this.handleIcons(rootGroup).then(() => {
            // Once images are loaded, add layers
            console.log("All images have been loaded?");

            // Plot the layers
            let allLayers = group.flattenUp();
            allLayers.forEach(layer => {
                this.plotLayer(group, layer);
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
        let collision = MapHandler.MAP.getSource(source.name);

        if(collision === null || collision === undefined) {
            // Clone the original source definition
            let options = {...source.definition};

            // Remove properties not expected by MapBox
            delete options["name"];
            if(options["metaFiles"]) delete options["metaFiles"];
            if(options["timeseriesFiles"]) delete options["timeseriesFiles"];

            // Add attributions if missing
            if(source.type !== "video" && source.type !== "image") {
                if(!options["attribution"]) {
                    options["attribution"] = "CMCL Innovations";
                }
            }

            // Add to the map
            MapHandler.MAP.addSource(source.name, options);
            console.info("Added source to MapBox map: " + source.name);
        }
    }

    /**
     * Adds the data layer to the map.
     * 
     * @param layer layer to add.
     */
    private addLayer(layer: DataLayer) {
        let collision = MapHandler.MAP.getLayer(layer.name);

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

            // Change 'name' field to 'id'
            options["id"] = layer.name;
            delete options["name"];

            // Add to the map
            MapHandler.MAP.addLayer(options);
            console.info("Added layer to MapBox map: " + layer.name);
        }
    }

    /**
     * 
     */
    private async handleIcons(rootGroup: DataGroup) {
        let iconFile = rootGroup.location + "/icons.json";

        // Load the JSON
        let json = await $.getJSON(iconFile, function(json) {
            return json;
        }).fail((error) => {
            console.warn("Could not read 'icons.json' file; it's an optional file so skipping...")
        });
        
        // Load images once JSON has loaded
        var promises = [];
        for(var key of Object.keys(json)) {

            // Create a promise that resolves once the image is loaded AND added
            let promise = new Promise((resolve, reject) => {
                let imageName = key;
                let imageFile = rootGroup.location + "/" + json[key];

                let hasImage = MapHandler.MAP.hasImage(imageName);
                if(!hasImage) {

                    MapHandler.MAP.loadImage(
                        imageFile,
                        (error, image) => {
                            if(error) {
                                console.log(error);
                                reject(error);
                            }

                            MapHandler.MAP.addImage(imageName, image);
                            resolve([]);
                        }
                    );
                }
            });
            promises.push(promise);
        }

        return Promise.all(promises);
    }

}