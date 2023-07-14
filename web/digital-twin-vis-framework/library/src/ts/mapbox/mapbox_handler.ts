/**
 * Concrete implementation of the MapHandler class that handles
 * a single Mapbox map instance.
 */
class MapHandler_Mapbox extends MapHandler {

    /**
     * Mapbox popup element.
     */
    public static POPUP;

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
        // Set the mapbox api key
        // @ts-ignore
        mapboxgl.accessToken = MapHandler.MAP_API;

        // Check the options for required parameters, provide defaults if missing
        let newOptions = (mapOptions !== null) ? mapOptions : {};
        if(!newOptions["container"]) newOptions["container"] = "map"
        if(!newOptions["center"]) newOptions["center"] = [-0.1280432939529419, 51.50805967151767];
        if(!newOptions["zoom"]) newOptions["zoom"] = 16;

        // Set the default style imagery
        newOptions["style"] = getDefaultImagery();

        if(MapHandler.MAP === null || MapHandler.MAP === undefined) {
            // Create new map (note the settings used here may be overriden when the map is loaded with data).
            // @ts-ignore
            MapHandler.MAP = new mapboxgl.Map(newOptions);
            MapHandler.MAP_OPTIONS = newOptions;

            // Setup mouse interactions
            MapHandler.MAP.on("click", (event) => this.handleClick(event, null));
            MapHandler.MAP.on("mousemove", (event) => this.handleMouse(event));

            // Create popup
             // @ts-ignore
            MapHandler_Mapbox.POPUP = new mapboxgl.Popup({
                closeButton: false,
                closeOnClick: false,
                maxWidth: "400px"
            });
        }  else {
            // Reinitialise state of existing map
            MapHandler.MAP.setStyle(newOptions["style"]);
            delete newOptions["container"];

            MapHandler.MAP.jumpTo(newOptions);
        }

        // Load custom search terms
        //MapboxUtils.loadSearchTerms();
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
            return MapboxUtils.isCMCLLayer(feature);
        });

        // Filter out duplicates (Mapbox can return these if a feature is split across a tile boundary)
        features = MapboxUtils.deduplicate(features);

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

            if(MapboxUtils.isCluster(feature)) {
                // Clicked on a clustered feature, handle as if multiple
                this.clickMultiple(features);

            } else {
                // Click on single feature
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
        await MapboxUtils.recurseFeatures(leafs, features);

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
            return MapboxUtils.isCMCLLayer(feature);
        });

        if(features.length === 0) {
            // Mouse no longer over any features
            MapHandler.MAP.getCanvas().style.cursor = '';
            if(MapHandler_Mapbox.POPUP !== null) MapHandler_Mapbox.POPUP.remove();

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

            if(layer != null && layer instanceof MapboxLayer) {
                if(feature !== null) MapboxUtils.showPopup(event, feature);
            } 
        } 
    }

    /**
     * Plot the contents of the input data group on the map.
     */
    public plotData(dataStore: DataStore) {
        // Remove all previous layers then sources
        let previousSources = new Set();

        MapHandler.MAP.getStyle().layers.forEach(function(layer) {
            // Only remove layers we've added
            if(layer.metadata?.attribution === "CMCL") {
                previousSources.add(layer.source);
                MapHandler.MAP.removeLayer(layer.id);
                console.log("REMOVED " + layer.id);
            }
        });
        previousSources.forEach(id => MapHandler.MAP.removeSource(id));

        // Get all layers from all groups
        let allLayers = [];
        dataStore.dataGroups.forEach(rootGroup => {
            let groupLayers = rootGroup.flattenDown();
            allLayers = allLayers.concat(groupLayers);
        });

        // Order them
        allLayers = allLayers.sort((a, b) => {
            if(a.order > b.order) return 1;
            if(a.order < b.order) return -1;
            return 0;
        });

        // Plot them
        allLayers.forEach(layer => this.plotLayer(null, layer));
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

            // Remove properties not expected by Mapbox
            if(options["id"]) delete options["id"];
            if(options["metaFiles"]) delete options["metaFiles"];
            if(options["timeseriesFiles"]) delete options["timeseriesFiles"];

            // Add attributions if missing
            if(source.type !== "video" && source.type !== "image") {
                if(!options["attribution"]) {
                    options["attribution"] = "CMCL";
                }
            }

            // Add to the map
            MapHandler.MAP.addSource(source.id, options);
            console.info("Added source to Mapbox map: " + source.id);
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
                options["metadata"]["attribution"] = "CMCL";
            }

            // Remove 'clickable' if specified
            if(options["clickable"]) {
                options["metadata"]["clickable"] = options["clickable"]
                delete options["clickable"]
            } else {
                options["metadata"]["clickable"] = true
            }

            // Remove 'treeable' if specified
            if(options["treeable"]) {
                options["metadata"]["treeable"] = options["treeable"]
                delete options["treeable"]
            } else {
                options["metadata"]["treeable"] = true
            }

            // Update to unique ID
            options["id"] = layer.id;

            // Remove fields not required by Mapbox
            delete options["name"];
            delete options["order"];

            // Add to the map
            MapHandler.MAP.addLayer(options);
            console.info("Added layer to Mapbox map '" + layer.id + "'.");
        }
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