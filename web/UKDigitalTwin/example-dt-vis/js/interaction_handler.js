/**
 * Registers and handles interactions with the MapBox map (e.g. mouse overs, 
 * location clicks, keyboard events).
 */
class InteractionHandler {

    #NO_ADDITIONAL = `
        <div style="height: 100%; background-color: lightgrey;">
            <p style="text-align: center; margin: 20px; position: relative; top: 50%; transform: translateY(-50%);">
                Select a group using the controls to the left of the map to view the
                group specific metadata for this location.
            </p>
        </div>
    `;

    _map;

    _dataRegistry;

    _panelHandler;

    _popup;

    _previousTab = "meta-tree-button";

    _timeseriesHandler;

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


    registerInteractions(layerName) {
        // Mouse enter
        this._map.on("mouseenter", layerName, (event) => {
            this._map.getCanvas().style.cursor = 'pointer';
            let feature = event.features[0];

            // Get correct co-ords
            var coordinates = feature.geometry.coordinates.slice();
            while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
                coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
            }

            // Get appropriate description for layer
            if(!feature.properties["displayName"] && !feature.properties["name"]) return;
            var description = feature.properties["displayName"];
            if(description == null) description = feature.properties["name"];

            var html = "<b>" + description + "</b></br>";
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

        // Mouse click
        this._map.on("click", layerName, (event) => {
            let feature = event.features[0];
            this.mouseClick(feature);
        });

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
        this._panelHandler.setTitle(title);

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
            allMetadata["Properties"] = fixedMeta;
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
            allMetadata["Transient Properties"] = transientMeta;
        });

        // Build tree once all metadata is added
        Promise.all([finalFixedPromise, finalAdditionalPromise]).then(() => {

            if(allMetadata == null || allMetadata.length == 0) {
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

            console.log(values);

            if(values == null || values.length == 0) {
                // No time series data
                console.log("A");

                document.getElementById("time-series-button").style.display = "none";
                this.openTreeTab("meta-tree-button", "meta-tree");
            } else {
                // Data present, show it
                console.log("B");
                
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
            });
        });
        return allPromises;
    }


}
// End of class.