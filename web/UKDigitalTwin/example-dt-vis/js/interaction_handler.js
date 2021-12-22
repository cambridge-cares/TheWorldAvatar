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

    _hoveredFeature = null;

    _lastClick;

    /**
     * Initialise a new interaction handler.
     * 
     * @param {*} map 
     * @param {*} dataRegistry 
     */
    constructor(map, dataRegistry, panelHandler, timeseriesHandler) {
        this._map = map;
        this._registry = dataRegistry;
        this._panelHandler = panelHandler;
        this._timeseriesHandler = timeseriesHandler;

        this._popup = DT.popup;

        this.registerInteractions();
    }


    /**
     * Register default mouse interactions with the input layer.
     * 
     * @param {string[]} layer [layer name, layer type]
     */
    registerInteractions() {

        // Mouse click
        this._map.on("click", (event) => {

            let features = this._map.queryRenderedFeatures(event.point);
            let feature = features.find(hit => manager._sourceHandler._currentSources.includes(hit.layer.source));
            if (feature) this.mouseClick(feature);

            // Filter to determine how many non-default, circle/symbol features are present
            let self = this;
            let siteFeatures = features.filter(feature => {
                let featureLayer = feature["layer"]["id"];
                if(featureLayer.includes("_clickable")) return false;
                if(featureLayer.includes("_arrows")) return false;

                let layer = self._map.getLayer(featureLayer);
                if(layer["type"] !== "circle" && layer["type"] !== "symbol") return false;
                return layer["metadata"]["provider"] === "cmcl";
            });

            if(siteFeatures.length == 1 && layerName.endsWith("_cluster")) {
                // If a cluster feature, let the user pick the leaf feature
                this.#handleClusterClick(siteFeatures[0], function(newFeature) {
                    self.mouseClick(layerName.replace("_cluster", ""), newFeature);
                });
            } else {
                // If more than one, let the use pick
                if(siteFeatures.length > 1) {
                    feature = this.#handleMultipleFeatures(siteFeatures, function(newFeature) {
                        // Trigger on chosen feature
                        if(newFeature != null) {    
                            self.mouseClick(newFeature["layer"]["id"], newFeature);
                        }
                    });
                } else {
                    self.mouseClick(layerName, feature);
                }
            }
        });

        this._map.on("mousemove", (event) => {

            let feature = this._map.queryRenderedFeatures(event.point)
                .find(hit => manager._sourceHandler._currentSources.includes(hit.layer.source));

            // Remove old feature's hover state
            if (this._hoveredFeature != null && (!feature || feature.id != this._hoveredFeature.id)) {
                // This can be false if we've just switched groups.
                if (manager._sourceHandler._currentSources.includes(this._hoveredFeature.layer.source)) {
                    this._map.setFeatureState(
                        { source: this._hoveredFeature.layer.source, id: this._hoveredFeature.id },
                        { hover: false }
                    );
                }
                this._hoveredFeature = null;
            }

            if (!feature) {
                this._map.getCanvas().style.cursor = 'default';
                this._popup.remove();
                return;
            }

            let html = "";
            let name = feature.properties["displayName"] ?? feature.properties["name"];
            this._map.getCanvas().style.cursor = "pointer";

            switch (feature.layer.type) {

                case "line":
                case "point":
                case "symbol":
                case "circle":

                    if (feature.layer.id.endsWith("_arrows")) return;

                    // Get correct co-ords
                    let coordinates = feature.geometry.coordinates.slice();
                    while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
                        coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
                    }

                    if (feature.layer.id.endsWith("_cluster")) {
                        html = "<h3>Multiple features</h3>";

                        let count = feature["properties"]["point_count_abbreviated"];
                        html += `There are a number of features (` + count + `) at this location. `;
                        html += "Click to show a list features and select an individual."
                    } else {
                        if (name == null) return;
                        // Build HTML for popup
                        html = "<h3>" + name + "</h3>";
                        if (feature.properties["description"]) {
                            html += feature.properties["description"] + "</br></br>"
                        }
                    }

                    if (coordinates.length == 2 && this.#isNumber(coordinates[0])) {
                        // Add coordinate details if a point
                        this._popup.setLngLat(coordinates).setHTML(html).addTo(this._map);
                    } else if (coordinates.length >= 2) {
                        let center = turf.centroid(feature)["geometry"]["coordinates"];
                        this._popup.setLngLat(center).setHTML(html).addTo(this._map);
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

                    if (name == null) return;

                    // Build HTML for popup
                    html = "<b>" + name + "</b></br>";
                    if (feature.properties["description"]) {
                        html += feature.properties["description"] + "</br></br>"
                    }

                    // Get coords for the center of the polygon
                    let center = turf.centroid(feature)["geometry"]["coordinates"];
                    this._popup.setLngLat(center).setHTML(html).addTo(this._map);

            }

        });

        console.log("INFO: Interactions have been registered.");
    }

    /**
     * 
     * @param {*} feature 
     */
    mouseClick(feature) {
        // Clear existing side panel content
        this._panelHandler.setContent("");

        // Hide the legend
        this._panelHandler.toggleLegend(false);

        // Show the title
        var title = feature.properties["displayName"];
        if (title == null) title = feature.properties["name"];
        if (title == null) title = "ID " + feature.id;

        if (feature["geometry"]["type"] === "Point") {
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
        if (sidePanel.classList.contains("collapsed")) {
            this._panelHandler.toggleExpansion();
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
        if (feature.properties["description"]) {
            beforeContainer.style.display = "block";
            beforeContainer.innerHTML = `<p>` + feature.properties["description"] + `</p>`;
        } else {
            beforeContainer.style.display = "none";
        }

        // Get the metadata
        var metaPromises = this.#findMeta(feature);

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
    #handleTimeseries(feature) {
        var timePromises = this.#findTimeSeries(feature);

        var self = this;
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
                document.getElementById("time-series-container").innerHTML = "";
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
     * Finds the metadata for the input map feature.
     * 
     * @param {JSONObject} feature selected map feature 
     */
    #findMeta(feature) {
        var metaGroup = this._registry.getGroup(DT.currentGroup);
        if (metaGroup == null) return;

        var allPromises = [];

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            let layerName = feature.layer["id"].replace("_clickable", "");

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
    #findTimeSeries(feature) {
        // For each currently selected leaf group
        let metaGroup = this._registry.getGroup(DT.currentGroup);
        if (metaGroup == null) return;

        var allPromises = [];

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            if (dataSet["name"] === feature.layer["id"]) {
                let timeFiles = dataSet["timeseriesFiles"];

                if (timeFiles != null) {

                    // Read each time file
                    for (var j = 0; j < timeFiles.length; j++) {
                        let metaDir = metaGroup["thisDirectory"];
                        let timeFile = metaDir + "/" + timeFiles[j];
                        console.log("INFO: Reading Additional timeseries JSON at " + timeFile);

                        // Load file asynchronously
                        var promise = $.getJSON(timeFile).then(json => {

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
     * @param {*} n 
     * @returns 
     */
    #isNumber(n) {
        return !isNaN(parseFloat(n)) && !isNaN(n - 0);
    }


}
// End of class.