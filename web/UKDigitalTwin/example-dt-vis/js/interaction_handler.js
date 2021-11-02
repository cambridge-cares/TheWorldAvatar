/**
 * Registers and handles interactions with the MapBox map (e.g. mouse overs, 
 * location clicks, keyboard events).
 */
class InteractionHandler {

    _map;

    _dataRegistry;

    _panelHandler;

    _popup;

    _previousTab = "meta-tree-button";

    _timeseriesHandler;

    _hoveredStateId = null;

    /**
     * Initialise a new interaction handler.
     * 
     * @param {*} map 
     * @param {*} dataRegistry 
     */
    constructor(map, dataRegistry, panelHandler, timeseriesHandler) {
        this._map = map;
        this._dataRegistry = dataRegistry;
        this._panelHandler = panelHandler;
        this._timeseriesHandler = timeseriesHandler;

        this._popup = DT.popup;
    }


    /**
     * Register default mouse interactions with the input layer.
     * 
     * @param {string[]} layer [layer name, layer type]
     */
    registerInteractions(layer) {
        let layerName = layer[0];
        let layerType = layer[1];
        let sourceName = this._map.getLayer(layerName).source;

        var lastFeature = null;

        // Interactions per layer type
        switch(layerType) {
            case "line":
               // No interactions for line features
            break;

            case "point":
                // Mouse click
                this._map.on("click", layerName, (event) => {
                    let feature = event.features[0];
                    this.mouseClick(feature);
                });

                // Mouse enter
                this._map.on("mouseenter", layerName, (event) => {
                    let feature = this._map.queryRenderedFeatures(event.point)[0];

                    // Change cursor
                    this._map.getCanvas().style.cursor = 'pointer';

                    // Get correct co-ords
                    var coordinates = feature.geometry.coordinates.slice();
                    while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
                        coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
                    }

                    // Get appropriate description for layer
                    if(!feature.properties["displayName"] && !feature.properties["name"]) {
                        return;
                    }
                    var name = feature.properties["displayName"];
                    if(name == null) name = feature.properties["name"];

                    // Build HTML for popup
                    var html = "<b>" + name + "</b></br>";
                    if(feature.properties["description"]) {
                        html += feature.properties["description"] + "</br></br>"
                    }

                    // Show popup
                    html += "<em>" + coordinates[1].toFixed(5) + ", " + coordinates[0].toFixed(5) + "</em>"
                    this._popup.setLngLat(coordinates).setHTML(html).addTo(this._map);
                });

                // Mouse exit
                this._map.on("mouseleave", layerName, (event) => {
                    this._map.getCanvas().style.cursor = '';
                    this._popup.remove();
                });
                break;

                case "fill":
                case "extrusion":
                case "polygon":
                    var lastFeature = null;

                    // When the user moves their mouse over the fill area
                    this._map.on('mousemove', layerName, (e) => {
                        var thisFeature = this._map.queryRenderedFeatures(e.point)[0];

                        if(lastFeature == null || thisFeature.id != lastFeature.id) {
                            lastFeature = thisFeature;

                            // Remove old feature's hover state
                            if (this._hoveredStateId !== null) {
                                this._map.setFeatureState(
                                    { source: sourceName, id: this._hoveredStateId },
                                    { hover: false }
                                );
                            }
                            
                            // Set hover state for this feature
                            this._hoveredStateId = e.features[0].id;
                            this._map.setFeatureState(
                                { source: sourceName, id: this._hoveredStateId },
                                { hover: true }
                            );

                            // Get appropriate description for layer
                            if(!thisFeature.properties["displayName"] && !thisFeature.properties["name"]) return;
                            var name = thisFeature.properties["displayName"];
                            if(name == null) name = thisFeature.properties["name"];

                            // Build HTML for popup
                            var html = "<b>" + name + "</b></br>";
                            if(thisFeature.properties["description"]) {
                                html += thisFeature.properties["description"] + "</br></br>"
                            }

                            // Get coords for the center of the polygon
                            var center = turf.centroid(thisFeature)["geometry"]["coordinates"];
                            this._popup.setLngLat(center).setHTML(html).addTo(this._map);
                        }
                    });

                    // Mouse exit
                    this._map.on("mouseleave", layerName, (event) => {
                        this._map.getCanvas().style.cursor = '';
                        this._popup.remove();
                        lastFeature = null;

                        if (this._hoveredStateId !== null) {
                            this._map.setFeatureState(
                                { source: sourceName, id: this._hoveredStateId },
                                { hover: false }
                            );
                        }
                        this._hoveredStateId = null;
                    });
                break;
        } 

        console.log("INFO: Interactions for layer '" + layerName + "' have been registered.");
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     */
    mouseClick(feature) {
        // Clear existing side panel content
        this._panelHandler.setContent("");

        // Hide the legend
        this._panelHandler.toggleLegend(false);
        
        // Show the title
        var title = feature.properties["displayName"];
        if(title == null) title = feature.properties["name"];
        if(title == null) title = "ID " + feature.id;

        if(feature["geometry"]["type"] === "Point") {
            var coordinates = feature.geometry.coordinates;

            var titleHTML = title + `
                <div class="tooltip">
                    <img src="./img/target.png" onclick="manager.zoomTo(` + coordinates + `);"/>
                    <span class="tooltiptext">Zoom to this location</span>
                </div>
            `;
            this._panelHandler.setTitle(titleHTML);
        } else {
            this._panelHandler.setTitle(title);
        }
       
        // Handle the metadata
        this.#handleMetadata(feature);

        // Handle the timeseries data
        this.#handleTimeseries(feature);

        // Ensure the previously opened tab is open
        document.getElementById(this._previousTab).click();

        // Expand the side panel if it's collapsed
        var sidePanel = document.getElementById("sidePanel");
        if(sidePanel.classList.contains("collapsed")) {
            this._panelHandler.toggleMode();
        }

        // Remember the currently selected feature
        DT.currentFeature = feature;
    }

    /**
     * Finds and displays the correct metadata for the input GeoJSON feature.
     * 
     * @param {JSONObject} feature selected GeoJSON feature.
     */
    #handleMetadata(feature) {
        // Build containers for metadata trees
        this.#buildMetadataContainers();

        var beforeContainer = document.getElementById("content-before");
        if(feature.properties["description"]) {
            beforeContainer.style.display = "block";
            beforeContainer.innerHTML = `<p>` + feature.properties["description"] + `</p>`;
        } else {
            beforeContainer.style.display = "none";
        }

        // Get the metadata
        var allMetadata = [];

        // Read the fixed metadata
        var fixedPromises = this.#findFixedMeta(feature);
        var finalFixedPromise = Promise.all(fixedPromises).then((values) => {
            var fixedMeta = [];
            values.forEach(jsonEntry => {
                Object.keys(jsonEntry).forEach(function(key) {
                    if(key !== "id") fixedMeta[key] = jsonEntry[key];
                });
            });

            if(fixedMeta.length > 0 || Object.keys(fixedMeta).length > 0) { 
                allMetadata["Properties"] = fixedMeta;
            }
        });

        // Read the additional metadata
        var additionalPromises = this.#findAdditionalMeta(feature);
        var finalAdditionalPromise = Promise.all(additionalPromises).then((values) => {
            var transientMeta = [];
            values.forEach(jsonEntry => {
                Object.keys(jsonEntry).forEach(function(key) {
                    if(key !== "id") transientMeta[key] = jsonEntry[key];
                });
            });

            if(transientMeta.length > 0 || Object.keys(transientMeta).length > 0) { 
                allMetadata["Transient Properties"] = transientMeta;
            }
        });

        // Build tree once all metadata is added
        Promise.all([finalFixedPromise, finalAdditionalPromise]).then(() => {
            document.getElementById("meta-tree").innerHTML = "";

            console.log(allMetadata);

            if(allMetadata == null || Object.keys(allMetadata).length == 0) {
                // Fallback to the GeoJSON properties
                var metaTree = JsonView.renderJSON(feature.properties, document.getElementById("meta-tree"));
                JsonView.expandChildren(metaTree);
            } else {
                // Show the metadata tree
                var metaTree = JsonView.renderJSON(allMetadata, document.getElementById("meta-tree"));
                JsonView.expandChildren(metaTree);
            }
        });
    }

    /**
     * Finds and displays the correct timeseries data for the input GeoJSON feature.
     * 
     * @param {JSONObject} feature selected GeoJSON feature.
     */
    #handleTimeseries(feature) {
        var fixedPromises = this.#findFixedTimeSeries(feature);
        var additionalPromises = this.#findAdditionalTimeSeries(feature);
        var allPromises = fixedPromises.concat(additionalPromises);

        var self = this;
        Promise.all(allPromises).then((values) => {

            if(values == null || values.length == 0) {
                // No time series data
                document.getElementById("time-series-button").style.display = "none";
                this.openTreeTab("meta-tree-button", "meta-tree");
            } else {
                // Data present, show it
                document.getElementById("time-series-button").style.display = "block";
                self._timeseriesHandler.parseData(values);
                self._timeseriesHandler.showData("time-series-container");
            }
        });
    }

    #buildMetadataContainers() {
        var html = `
            <div id="content-before"></div>
            <div class="json-tree-tabs">
                <button id="meta-tree-button" class="tablinks" onclick="manager.openTreeTab(this.id, 'meta-tree')">Metadata</button>
                <button id="time-series-button" class="tablinks" onclick="manager.openTreeTab(this.id, 'time-series-container')">Time Series</button>
            </div>
            <div id="meta-tree" style="margin-top: 10px;" class="tabcontent"></div>
            <div id="time-series-container" style="margin-top: 10px;" class="tabcontent"></div>
        `;
        this._panelHandler.setContent(html);
    }

    openTreeTab(tabButtonName, tabName) {
        // Declare all variables
        var i, tabcontent, tablinks;
      
        // Get all elements with class="tabcontent" and hide them
        tabcontent = document.getElementsByClassName("tabcontent");
        for (i = 0; i < tabcontent.length; i++) {
            tabcontent[i].style.display = "none";
        }
      
        // Get all elements with class="tablinks" and remove the class "active"
        tablinks = document.getElementsByClassName("tablinks");
        for (i = 0; i < tablinks.length; i++) {
            tablinks[i].className = tablinks[i].className.replace(" active", "");
        }
      
        // Show the current tab, and add an "active" class to the button that opened the tab
        document.getElementById(tabName).style.display = "block";
        document.getElementById(tabButtonName).className += " active";

        this._previousTab = tabButtonName;
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     * @param {*} callback 
     */
    #findFixedMeta(feature) {
        let metaDir = this._dataRegistry.getFixedDirectory();
        let datasets = this._dataRegistry.fixedMeta["dataSets"];
        let allPromises = [];

        for(var i = 0; i < datasets.length; i++) {
            // Check if the layer name is the same
            if(datasets[i]["name"] === feature.layer["id"]) {
                let metaFiles = datasets[i]["metaFiles"];
                if(metaFiles == null || metaFiles.length == 0) continue;

                // Load each listed meta file
                for(var j = 0; j < metaFiles.length; j++) {
                    let metaFile = metaDir + "/" + metaFiles[j];
                    console.log("INFO: Reading Fixed metadata JSON at " + metaFile);

                    // Load file asynchronously
                    var promise = $.getJSON(metaFile).then(json => {

                        // Once read, only return the node with the matching feature id
                        for(var i = 0; i < json.length; i++) {
                            if(json[i]["id"] == feature.id) {
                                return json[i];
                            }
                        }
                    });

                    // Pool promise
                    allPromises.push(promise);
                }
            }
        }
        return allPromises;
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     */
    #findAdditionalMeta(feature) {
        var allPromises = [];

        // For each currently selected leaf group
        DT.currentAdditionals.forEach(groupListing => {

            let metaDir = this._dataRegistry.getAdditionalDirectory(groupListing);
            let metaGroup = this._dataRegistry.getAdditionalGroup(groupListing);
            if(metaDir == null || metaGroup == null) return;
           
            metaGroup["dataSets"].forEach(dataSet => {
                // Check if the layer name is the same
                if(dataSet["name"] === feature.layer["id"]) {
                    let metaFiles = dataSet["metaFiles"];

                        if(metaFiles != null) {

                        // Load each listed meta file
                        for(var j = 0; j < metaFiles.length; j++) {
                            let metaFile = metaDir + "/" + metaFiles[j];
                            console.log("INFO: Reading Additional metadata JSON at " + metaFile);

                            // Load file asynchronously
                            var promise = $.getJSON(metaFile).then(json => {

                                // Once read, only return the node with the matching feature id
                                for(var i = 0; i < json.length; i++) {
                                    if(json[i]["id"] == feature.id) {
                                        return json[i];
                                    }
                                }
                            });

                            // Pool promise
                            allPromises.push(promise);
                        }
                    }
                }
            });
        });

        return allPromises;
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     * @param {*} callback 
     */
    #findFixedTimeSeries(feature) {
        let metaDir = this._dataRegistry.getFixedDirectory();
        let datasets = this._dataRegistry.fixedMeta["dataSets"];
        var allPromises = [];

        for(var i = 0; i < datasets.length; i++) {
            // Check if the layer name is the same
            if(datasets[i]["name"] === feature.layer["id"]) {
                let timeFiles = datasets[i]["timeseriesFiles"];
                if(timeFiles == null || timeFiles.length == 0) continue;

                for(var j = 0; j < timeFiles.length; j++) {
                    let timeFile = metaDir + "/" + timeFiles[j];
                    console.log("INFO: Reading Fixed timeseries JSON at " + timeFile);

                    // Load file asynchronously
                    var promise = $.getJSON(timeFile).then(json => {

                        // Once read, only return the node with the matching feature id
                        for(var i = 0; i < json.length; i++) {
                            if(json[i]["id"] == feature.id) {
                                return json[i];
                            }
                        }
                    });

                    // Pool promise
                    allPromises.push(promise);
                }
            }
        }
        return allPromises;
    }

     /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     * @param {*} callback 
     */
      #findAdditionalTimeSeries(feature) {
        var allPromises = [];

         // For each currently selected leaf group
        DT.currentAdditionals.forEach(groupListing => {

            let metaDir = this._dataRegistry.getAdditionalDirectory(groupListing);
            let metaGroup = this._dataRegistry.getAdditionalGroup(groupListing);
            if(metaDir == null || metaGroup == null) return;
        
            metaGroup["dataSets"].forEach(dataSet => {
                // Check if the layer name is the same
                if(dataSet["name"] === feature.layer["id"]) {
                    let timeFiles = dataSet["timeseriesFiles"];

                    if(timeFiles != null) {

                        // Read each time file
                        for(var j = 0; j < timeFiles.length; j++) {
                            let timeFile = metaDir + "/" + timeFiles[j];
                            console.log("INFO: Reading Additional timeseries JSON at " + timeFile);

                            // Load file asynchronously
                            var promise = $.getJSON(timeFile).then(json => {

                                // Once read, only return the node with the matching feature id
                                for(var i = 0; i < json.length; i++) {
                                    if(json[i]["id"] == feature.id) {
                                        return json[i];
                                    }
                                }
                            });
                        

                            // Pool promise
                            allPromises.push(promise);
                        }
                    }
                }
            });
        });
        return allPromises;
    }


}
// End of class.