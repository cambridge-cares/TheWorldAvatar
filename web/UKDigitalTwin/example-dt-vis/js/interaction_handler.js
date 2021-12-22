/**
 * Registers and handles interactions with the MapBox map (e.g. mouse overs, 
 * location clicks, keyboard events).
 */
class InteractionHandler {

    _map;

    _registry;

    _panelHandler;

    _popup;

    _previousTab = "meta-tree-button";

    _timeseriesHandler;

    _layerHander;

    _hoveredStateId = null;

    _lastClick;

    _selectionCallbacks = {};

    /**
     * Initialise a new interaction handler.
     * 
     * @param {*} map 
     * @param {*} dataRegistry 
     */
    constructor(map, dataRegistry, panelHandler, timeseriesHandler, layerHandler) {
        this._map = map;
        this._registry = dataRegistry;
        this._panelHandler = panelHandler;
        this._timeseriesHandler = timeseriesHandler;
        this._layerHander = layerHandler;
        this._popup = DT.popup;

        this.registerInteractions();
    }

    /**
     * Add a callback what will fire after a feature within the 
     * input MapBox layer has been selected.
     * 
     * @param {String} layerName MapBox layerID
     * @param {Function} callback function to execute 
     */
    addSelectionCallback(layerName, callback) {
        this._selectionCallbacks[layerName] = callback;
    }

    /**
     * Register default mouse interactions with the input layer.
     * 
     * @param {string[]} layer [layer name, layer type]
     */
    registerInteractions() {

        // Mouse click
        this._map.on("click", (event) => {

            let features = this._map.queryRenderedFeatures(event.point).filter(feature =>
                feature.layer.metadata?.provider === "cmcl" && feature.layer.metadata?.clickable);
            if (features.length == 0) return;

            // Filter to determine how many non-default, circle/symbol features are present
            let siteFeatures = features.filter(feature =>
                !feature.layer.id.endsWith("_clickable") && !feature.layer.id.endsWith("_arrows") &&
                (feature.layer.type == "symbol" || feature.layer.type == "circle"));

            if (siteFeatures.length == 0) {
                this.mouseClick(features[0]);
            } else if (siteFeatures.length == 1) {
                // Note that the single clusters redirect to #handleClusterClick is already captured in the mouseClick method.
                this.mouseClick(siteFeatures[0]);
            } if (siteFeatures.length > 1) {
                // If more than one, let the user pick
                // Note the lambda is necessary to capture "this".
                this.#handleMultipleFeatures(siteFeatures, (selectedFeature) => this.mouseClick(selectedFeature));
            }
        });

        this._map.on("mousemove", (event) => {

            let feature = this._map.queryRenderedFeatures(event.point)
                .find(features => features.layer.metadata?.provider === "cmcl");

            // Remove old feature's hover state
            if (this._hoveredFeature != null && (!feature || feature != this._hoveredFeature)) {
                this._map.setFeatureState(
                    { source: this._hoveredFeature.layer.source, id: this._hoveredFeature.id },
                    { hover: false }
                );
                this._hoveredFeature = null;
            }

            if (!feature) {
                this._map.getCanvas().style.cursor = 'default';
                this._popup.remove();
                return;
            }

            let html = "";
            let name = feature.properties["displayName"] ?? feature.properties["name"] ?? ("ID " + feature.id);
            this._map.getCanvas().style.cursor = "pointer";

            switch (feature.layer.type) {

                case "line":
                case "point":
                case "symbol":
                case "circle":

                    if (feature.layer.id.endsWith("_arrows")) return;

                    if (feature.layer.id.endsWith("_cluster")) {
                        html = "<h3>Multiple features</h3>";
                        let count = feature["properties"]["point_count_abbreviated"];
                        html += `There are a number of features (` + count + `) at this location. `;
                        html += "Click to show a list features and select an individual."
                    } else {
                        // Build HTML for popup
                        html = "<h3>" + name + "</h3>";
                        if (feature.properties["description"]) {
                            html += feature.properties["description"] + "</br></br>"
                        }
                    }

                    if (feature.geometry) {
                        // Get correct co-ords
                        let coordinates = feature.geometry.coordinates.slice();
                        while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
                            coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
                        }
                        if (coordinates.length == 2 && this.#isNumber(coordinates[0])) {
                            // Add coordinate details if a point
                            this._popup.setLngLat(coordinates).setHTML(html).addTo(this._map);
                        } else if (coordinates.length >= 2) {
                            let center = turf.centroid(feature)["geometry"]["coordinates"];
                            this._popup.setLngLat(center).setHTML(html).addTo(this._map);
                        }
                    }

                    break;

                case "fill":
                case "extrusion":
                case "fill-extrusion":
                case "polygon":

                    // Set hover state for this feature
                    this._hoveredFeature = feature;
                    this._map.setFeatureState(
                        { source: feature.layer.source, id: feature.id },
                        { hover: true }
                    );

                    // Build HTML for popup
                    html = "<b>" + name + "</b></br>";
                    if (feature.properties["description"]) {
                        html += feature.properties["description"] + "</br></br>"
                    }

                    if (feature.geometry) {
                        let centroid = turf.centroid(feature)["geometry"]["coordinates"];
                        this._popup.setLngLat(centroid).setHTML(html).addTo(this._map);
                    }

            }

        });

        console.log("INFO: Interactions have been registered.");
    }

    /**
     * Given a cluster feature, this method extracts its leaf features and passes
     * their details onto the handleMultipleFeatures() method.
     *  
     * @param {JSONObject} feature 
     * @param {Function} callback 
     */
    #handleClusterClick(feature, callback) {
        let sourceName = feature["layer"]["source"];
        let source = this._map.getSource(sourceName);

        source.getClusterLeaves(feature.id, 999, 0, (error, features) => {
            if (error) {
                console.log("ERROR: Could not determine leaf features within cluster.");
                console.log(error);

            } else if (features != null) {
                features.forEach(leaf => {
                    leaf["layer"] = [];
                    leaf["source"] = sourceName;
                    leaf["layer"]["id"] = feature["layer"]["id"].replace("_cluster", "");
                });
                this.#handleMultipleFeatures(features, callback);
            }
        });
    }

    /**
     * Given an array of possible GeoJSON features, create controls to allow the user
     * to selet an individual feature, then return it.
     * 
     * @param {JSONObject[]} features array of possible features. 
     * 
     * @returns selected GeoJSON feature
     */
    #handleMultipleFeatures(features, callback) {
        let html = `
            <div style="padding: 15px">
                <p>Multiple features are located at these coordinates, please choose which
                feature you'd like to select using the drop-down boxes below.</p>
                <br/><br/>
                <label for="select-feature">Feature:</label>
                <select name="features" id="select-feature" style>
                <option value="" disabled selected>Select a feature...</option>
        `;

        for (var i = 0; i < features.length; i++) {
            let displayName;
            if (features[i].layer.id.endsWith("_cluster")) {
                displayName = "Cluster of " + features[i]["properties"]["point_count_abbreviated"] + " features from '";
                displayName += features[i]["layer"]["id"].replace("_cluster", "") + "' layer."
            } else {
                displayName = features[i]["properties"]["displayName"];
            }

            html += `
                <option value="` + i + `">` + displayName + `</option>
            `;
        };
        html += `</select></div>`;

        this._panelHandler.setTitle("<h3>Multiple Features</h3>");
        this._panelHandler.setContent(html);
        this._panelHandler.toggleLegend(false);
        document.getElementById("footerContainer").style.display = "block";

        let selectElement = document.getElementById("select-feature");
        selectElement.addEventListener("click", function () {
            if (callback != null) {
                callback(features[selectElement.value]);
            }
        });
    }

    /**
     * 
     * @param {*} feature 
     */
    mouseClick(feature) {

        if (feature == null) return;
        let layerName = feature.layer.id;

        if (layerName.endsWith("_cluster")) {
            // If a cluster feature, let the user pick the leaf feature
            this.#handleClusterClick(feature, (newFeature) => this.mouseClick(newFeature));
        }

        // Clear existing side panel content
        this._panelHandler.setContent("");

        // Hide the legend
        this._panelHandler.toggleLegend(false);

        // Show the title
        var title = feature.properties["displayName"];
        if (title == null) title = feature.properties["name"];
        if (title == null) title = "ID " + feature.id;
        title = "<h3>" + title + "</h3>";

        if (feature.geometry?.type === "Point") {
            var coordinates = feature.geometry.coordinates;
            var html = `
                <table width="100%">
                    <tr>
                        <td width="90%">` + title + `</td>
                        <td width="10%">
                            <div class="tooltip">
                                <img src="./img/target.png" onclick="manager.zoomTo(` + coordinates + `);"/>
                                <span class="tooltiptext">Zoom to this location</span>
                            </div>
                        </td>
                    </tr>
                </table>
            `;
            this._panelHandler.setTitle(html);
        } else {
            this._panelHandler.setTitle(title);
        }

        // Handle the metadata
        this.#handleMetadata(feature, layerName);

        // Handle the timeseries data
        this.#handleTimeseries(feature, layerName);

        // Ensure the previously opened tab is open
        document.getElementById(this._previousTab).click();

        // Expand the side panel if it's collapsed
        var sidePanel = document.getElementById("sidePanel");
        if (sidePanel.classList.contains("collapsed")) {
            this._panelHandler.toggleExpansion();
        }
        document.getElementById("footerContainer").style.display = "block";
        document.getElementById("contentContainer").style.flexGrow = 1;

        DT.currentFeature = feature;

        // Fire optional selection callback
        let callback = this._selectionCallbacks[layerName];
        if (callback != null) {
            callback(feature);
        } else {
            callback = this._selectionCallbacks["*"];
            if (callback != null) callback(feature);
        }
    }

    /**
     * Finds and displays the correct metadata for the input GeoJSON feature.
     * 
     * @param {JSONObject} feature selected GeoJSON feature.
     */
    #handleMetadata(feature, layerName) {
        // Build containers for metadata trees
        this.#buildMetadataContainers();

        var beforeContainer = document.getElementById("content-before");
        if (feature.properties["description"]) {
            beforeContainer.style.display = "block";
            beforeContainer.innerHTML = `<p>` + feature.properties["description"] + `</p>`;
        } else {
            beforeContainer.style.display = "none";
        }

        // Get the metadata
        var metaPromises = this.#findMeta(feature, layerName);

        // Build tree once all metadata is added
        Promise.all(metaPromises).then((values) => {
            // Combine all meta entries into single tree
            var combinedMeta = [];
            values.forEach(jsonEntry => {
                Object.keys(jsonEntry).forEach(function (key) {
                    if (key !== "id") combinedMeta[key] = jsonEntry[key];
                });
            });

            // Show the metadata
            if (Object.keys(combinedMeta).length == 0) {
                // No metadata
                document.getElementById("meta-tree").innerHTML = `
                    <div id="no-meta-container">
                        <p>No metadata available for this location.</p>
                    </div>
                `;
            } else {
                document.getElementById("meta-tree").innerHTML = "";
                var metaTree = JsonView.renderJSON(combinedMeta, document.getElementById("meta-tree"));
                JsonView.expandChildren(metaTree);
            }
        });
    }

    /**
     * Finds and displays the correct timeseries data for the input GeoJSON feature.
     * 
     * @param {JSONObject} feature selected GeoJSON feature.
     */
    #handleTimeseries(feature, layerName) {
        var timePromises = this.#findTimeSeries(feature, layerName);

        Promise.all(timePromises).then((values) => {
            if (values == null || values.length == 0 || values[0] == null) {
                // No time series data
                document.getElementById("time-series-container").innerHTML = `
                    <div id="no-meta-container">
                        <p>No timeseries available for this location.</p>
                    </div>
                `;
            } else {
                // Data present, show it
                var flatEntries = [].concat(...values);
                document.getElementById("time-series-container").innerHTML = "";
                this._timeseriesHandler.parseData(flatEntries);
                this._timeseriesHandler.showData("time-series-container");
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
     * Finds the metadata for the input map feature.
     * 
     * @param {JSONObject} feature selected map feature 
     */
    #findMeta(feature, layerName) {
        var metaGroup = this._registry.getGroup(DT.currentGroup);
        if (metaGroup == null) return;

        var allPromises = [];

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            layerName = layerName.replace("_clickable", "");

            if (dataSet["name"] === layerName) {
                let metaFiles = dataSet["metaFiles"];

                if (metaFiles != null) {

                    // Load each listed meta file
                    for (var j = 0; j < metaFiles.length; j++) {
                        let metaDir = metaGroup["thisDirectory"];
                        let metaFile = metaDir + "/" + metaFiles[j];
                        console.log("INFO: Reading metadata JSON at " + metaFile);

                        // Load file asynchronously
                        var promise = $.getJSON(metaFile).then(json => {
                            // Once read, only return the node with the matching feature id
                            for (var i = 0; i < json.length; i++) {
                                if (json[i]["id"] == feature.id) {
                                    return json[i];
                                }
                            }
                        });

                        // Pool promises
                        allPromises.push(promise);
                    }
                }
            }
        });

        return allPromises;
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     * @param {*} callback 
     */
    #findTimeSeries(feature, layerName) {
        // For each currently selected leaf group
        let metaGroup = this._registry.getGroup(DT.currentGroup);
        if (metaGroup == null) return;

        var allPromises = [];

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            if (dataSet["name"] === layerName) {
                let timeFiles = dataSet["timeseriesFiles"];

                if (timeFiles != null) {

                    // Read each time file
                    for (var j = 0; j < timeFiles.length; j++) {
                        let metaDir = metaGroup["thisDirectory"];
                        let timeFile = metaDir + "/" + timeFiles[j];
                        console.log("INFO: Reading Additional timeseries JSON at " + timeFile);

                        // Load file asynchronously
                        var promise = $.getJSON(timeFile).then(json => {
                            var timeSeriesNodes = [];

                            // Once read, only return the node with the matching feature id
                            for (var i = 0; i < json.length; i++) {
                                if (json[i]["id"] == feature.id) {
                                    timeSeriesNodes.push(json[i]);
                                }
                            }
                            return timeSeriesNodes;
                        });

                        // Pool promises
                        allPromises.push(promise);
                    }
                }
            }
        });
        return allPromises;
    }

    /**
     * 
     * @param {*} n 
     * @returns 
     */
    #isNumber(n) {
        return !isNaN(parseFloat(n)) && !isNaN(n - 0);
    }


}
// End of class.