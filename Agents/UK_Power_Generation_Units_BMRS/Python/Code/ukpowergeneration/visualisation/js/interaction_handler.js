/**
 * Registers and handles interactions with the MapBox map (e.g. mouse overs, 
 * location clicks, keyboard events).
 * 
 * TODO: This class could do with some more refactoring, perhaps splitting
 * into multiple classes.
 */
class InteractionHandler {

    // Mapbox map
    _map;

    // MapBox popup
    _popup;

    // Data registry
    _registry;

    // Side panel handler
    _panelHandler;

    // Time series handle
    _timeseriesHandler;

    // Layer Handler
    _layerHander;

    // Name of previously opened tab
    _previousTab = "meta-tree-button";

    // ID of hovered feature
    _hoveredStateId = null;

    // Time of last click
    _lastClick;

    // Callback after feature selection
    _selectionCallbacks = {};
    
    // Cached meta data
    _cachedMetadata = {};

    // Cached timeseries
    _cachedTimeseries = {};

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
    registerInteractions(layer) {
        let layerName = layer[0];
        let layerType = layer[1];
        let sourceName = this._map.getLayer(layerName).source;

        var lastFeature = null;

        // Mouse click
        this._map.on("click", layerName, (event) => {
            if(!DT.clickEvents) return;
            if(layerName.endsWith("_arrows")) {
                return;
            }

             // Fudge to ensure that only one click per 500ms
            let thisClick = Date.now();
            if(this._lastClick != null) {
                if(Math.abs(thisClick - this._lastClick) < 500) {
                    return;
                }
            }
            this._lastClick = thisClick;

            // Get all visible features under the mouse click
            let features = this._map.queryRenderedFeatures(event.point);
            let feature = features[0];

            // Reset the side panel
            this._panelHandler.setContent("");

            // Filter to determine how many non-default, circle/symbol features are present
            let self = this;
            let siteFeatures = features.filter(feature => {
                let featureLayer = feature["layer"]["id"];
                
                // Filter out layers of specific types
                if(featureLayer.includes("_clickable")) return false;
                if(featureLayer.includes("_arrows")) return false;
                if(featureLayer.includes("-highlight")) return false;
                if(featureLayer.includes("-focus")) return false;

                // Filter out invisible layers
                if(self._map.getLayoutProperty(featureLayer, "visibility") === "none") {
                    return false;
                }

                let layer = self._map.getLayer(featureLayer);
                if(layer["type"] !== "circle" && layer["type"] !== "symbol") return false;
                if(layer["metadata"] && layer["metadata"]["provider"] === "cmcl") return true;
                return false;
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

        // Interactions per layer type
        switch(layerType) {

            case "line":
            case "point":
            case "symbol":
                // Mouse enter
                this._map.on("mouseenter", layerName, (event) => {
                    if(layerName.endsWith("_arrows")) {
                        return;
                    }

                    let feature = this._map.queryRenderedFeatures(event.point)[0];
                    if(feature == null || feature.geometry == null) return;

                    // Change cursor
                    if(DT.clickEvents) this._map.getCanvas().style.cursor = 'pointer';

                    // Get correct co-ords
                    var coordinates = feature.geometry.coordinates.slice();
                    while (Math.abs(event.lngLat.lng - coordinates[0]) > 180) {
                        coordinates[0] += event.lngLat.lng > coordinates[0] ? 360 : -360;
                    }

                    let html = "";
                    if(layerName.endsWith("_cluster")) {
                        html = "<h3>Multiple features</h3>";

                        let count = feature["properties"]["point_count_abbreviated"];
                        html += `There are `+ count + ` features at this location. `;
                        html += "Click to show a list of these features."
                    } else {
                        // Get appropriate description for feature
                        var name = feature.properties["displayName"];
                        if(name == null) name = feature.properties["name"];
                        if(name == null) return;

                        // Build HTML for popup
                        html = "<h3>" + name + "</h3>";
                        if(feature.properties["description"]) {
                            html += feature.properties["description"] + "</br></br>"
                        }
                    }

                    if(coordinates.length == 2 && this.#isNumber(coordinates[0])) {
                        // Add coordinate details if a point
                        this._popup.setLngLat(coordinates).setHTML(html).addTo(this._map);

                    } else if(coordinates.length >= 2) {
                        // Not a point, determine center then show popup
                        // let centroid = turf.centroid(feature);
                        // let popupLoc = centroid["geometry"]["coordinates"];
                        this._popup.setLngLat(event.lngLat).setHTML(html).addTo(this._map);
                    }
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

                    // Mouse enter
                    this._map.on("mouseenter", layerName, (event) => {
                        // Change cursor
                        if(!DT.clickEvents) return;
                        if(layerName.endsWith("_arrows")) {
                            return;
                        }
                        this._map.getCanvas().style.cursor = 'pointer';
                    });

                    // When the user moves their mouse over the fill area
                    this._map.on('mousemove', layerName, (e) => {
                        if(layerName.endsWith("_arrows")) {
                            return;
                        }

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
            if(error) {
                console.log("ERROR: Could not determine leaf features within cluster.");
                console.log(error);

            } else if(features != null) {
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
     * Given an array of possible GeoJSON features, this function create a dropdown control
     * to allow the user to selet an individual feature. This also supports arrays of clustered
     * features.
     * 
     * @param {JSONObject[]} features array of possible features. 
     * @param {Function} callback to fire with selected individual feature.
     */
    #handleMultipleFeatures(features, callback) {
        // Base HTML for control
        let html = `
            <div id="first-feature-select" style="margin-bottom: 10px;">
                <select name="features" id="select-feature" style="width: 90%;">
                    <option value="" disabled selected>Select a feature...</option>
        `;

        // Loop over features/clusters to build options for the dropdown.
        for(var i = 0; i < features.length; i++) {

            // Get (or construct) the feature's display name
            let displayName = features[i]["properties"]["displayName"];

            if(displayName == null) {
                // No display name, this will be a cluster then
                displayName = features[i]["properties"]["point_count_abbreviated"] + " features from '";

                // Get the internal layer ID and use the tree to determine the user facing name
                let layerID = features[i]["layer"]["id"].replace("_cluster", "");
                let layerName = null;

                for (const [key, value] of Object.entries(DT.treeDictionary)) {
                    if(value.includes(layerID)) {
                        layerName = key;
                    }
                }
                
                // Append layer name
                if(layerName == null) layerName = layerID;
                displayName += layerName + "' layer."
            }

            // The DT.selectColorer is an optional function (set externally) that when passed a feature
            // will return a CSS color string to use for any select options representing that feature.
            let color = "black";
            if(DT.selectColorer != null) {
                color = DT.selectColorer(features[i]);
            }

            // Append option to select control
            html += `
                <option style="color: ` + color + `;" value="` + i + `">` + displayName + `</option>
            `;
        };

        // Finish HTML
        html += `
                </select>
            </div>
            <div id="second-feature-select">
            </div>
        `;

        // Add the select option to the side panel
        this._panelHandler.setTitle("<h3>Multiple Features Selected</h3>");
        this._panelHandler.toggleLegend(false);

        let selectElement = document.querySelector("#first-feature-select select");
        if(selectElement === null) {
            this._panelHandler.setContent(`
                <p>Multiple features (or multiple clusters of features) are located at these coordinates,
                please choose which individual feature you'd like to select using the drop-down boxes below.</p>
                <br/><br/>`
                + html
            );

            selectElement = document.querySelector("#first-feature-select select");
        } else {
            let sidePanel = document.getElementById("contentContainer");
            let container = sidePanel.querySelector("#second-feature-select");
            container.innerHTML = html;

            selectElement = document.querySelector("#second-feature-select select");
        }

        // Show the return option
        document.getElementById("footerContainer").style.display = "block";

        if(selectElement != null) {
            selectElement.addEventListener("click", function() {
                if(callback != null) {
                    // Fire callback on option select
                    callback(features[selectElement.value]);
                }
            });
        }
    }

    /**
     * Fired when a feature is selected.
     * 
     * @param {String} layerName name of layer containing feature
     * @param {JSONObject} feature selected feature
     */
    mouseClick(layerName, feature) {
        if(feature == null) return;

        let self = this;
        if(layerName.endsWith("_cluster")) {
            // If a cluster feature, let the user pick the leaf feature
            this.#handleClusterClick(feature, function(newFeature) {
                self.mouseClick(layerName.replace("_cluster", ""), newFeature);
            });
            return;
        } 
        
        // Hide the legend
        this._panelHandler.toggleLegend(false);
        
        // Show the title
        var title = feature.properties["displayName"];
        if(title == null) title = feature.properties["name"];
        if(title == null) title = "ID " + feature.id;
        title = "<h3>" + title + "</h3>";

        if(feature["geometry"]["type"] === "Point") {
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
        if(sidePanel.classList.contains("collapsed")) {
            this._panelHandler.toggleExpansion();
        }
        document.getElementById("footerContainer").style.display = "block";
        document.getElementById("contentContainer").style.flexGrow = 1;

        DT.currentFeature = feature;

        // Fire optional selection callback
        let callback = this._selectionCallbacks[layerName];
        if(callback != null) {
            callback(feature);
        } else {
            callback = this._selectionCallbacks["*"];
            if(callback != null) callback(feature);
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

        // Loading text
        document.getElementById("meta-tree").innerHTML = `
            <div id="no-meta-container">
                <p>Loading metadata, please wait...</p>
            </div>
        `;

        var beforeContainer = document.getElementById("content-before");
        if(feature.properties["description"]) {
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
            if(values != null) {
                values.forEach(jsonEntry => {
                    Object.keys(jsonEntry).forEach(function(key) {
                        if(key !== "id") combinedMeta[key] = jsonEntry[key];
                    });
                });
            }

            // Show the metadata
            if(Object.keys(combinedMeta).length == 0) {
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
                JsonView.selectiveCollapse(metaTree); // selectively collapse nodes with the collapse field set to true
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

        var self = this;
        Promise.all(timePromises).then((values) => {
            if(values == null || values.length == 0 || values[0] == null) {
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
                self._timeseriesHandler.parseData(flatEntries);
                self._timeseriesHandler.showData("time-series-container");
            }
        });
    }

    /**
     * Builds the HTML container used to house the metadata tree and 
     * timeseries controls.
     */
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

    /**
     * Programatically select the metadata or timeseries tabs.
     * 
     * @param {String} tabButtonName 
     * @param {String} tabName 
     */
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
     * @param {String} layerName name of layer containing feature
     */
    #findMeta(feature, layerName) {
        var metaGroup = this._registry.getGroup(DT.currentGroup);
        if(metaGroup == null) return;

        let self = this;
        var allPromises = [];

        // Once read, return the nodes with the matching feature id
        let postRead = function(json) {
            for(var i = 0; i < json.length; i++) {
                if(json[i]["id"] == feature.id) {
                    return json[i];
                }
            }
        }

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            layerName = layerName.replace("_clickable", "");

            if(dataSet["name"] === layerName) {
                let metaFiles = dataSet["metaFiles"];
                if(metaFiles != null) {

                    // Load each listed meta file
                    for(var j = 0; j < metaFiles.length; j++) {
                        let metaDir  = metaGroup["thisDirectory"];
                        let metaFile = metaDir + "/" + metaFiles[j];

                        var promise;
                        if(self._cachedMetadata[metaFile]) {
                            // Use cached version
                            let json = self._cachedMetadata[metaFile];
                            promise = new Promise((resolve, reject) => {
                                resolve(postRead(json));
                            });
                        } else {
                            // Load file asynchronously
                            promise = $.getJSON(metaFile).then(json => {
                                self._cachedMetadata[metaFile] = json;
                                return postRead(json);
                            });
                        }

                        // Pool promises
                        allPromises.push(promise);
                    }
                }
            }
        });

        return allPromises;
    }
    
    /**
     * Finds the timeseries data for the input map feature.
     * 
     * @param {JSONObject} feature selected map feature 
     * @param {String} layerName name of layer containing feature
     */
    #findTimeSeries(feature, layerName) {
        // For each currently selected leaf group
        let metaGroup = this._registry.getGroup(DT.currentGroup);
        if(metaGroup == null) return;
    
        let self = this;
        var allPromises = [];

        // Once read, return the nodes with the matching feature id
        let postRead = function(json) {
            var timeSeriesNodes = [];
            for(var i = 0; i < json.length; i++) {
                if(json[i]["id"] == feature.id) {
                    timeSeriesNodes.push(json[i]);
                }
            }
            return timeSeriesNodes;
        }

        metaGroup["dataSets"].forEach(dataSet => {
            // Check if the layer name is the same
            if(dataSet["name"] === layerName) {
                let timeFiles = dataSet["timeseriesFiles"];

                if(timeFiles != null) {

                    // Read each time file
                    for(var j = 0; j < timeFiles.length; j++) {
                        let metaDir = metaGroup["thisDirectory"];
                        let timeFile = metaDir + "/" + timeFiles[j];

                        var promise;
                        if(self._cachedTimeseries[timeFile]) {
                            // Use cached version
                            let json = self._cachedTimeseries[timeFile];
                            promise = new Promise((resolve, reject) => {
                                resolve(postRead(json));
                            });
                        } else {
                            // Load file asynchronously
                            promise = $.getJSON(timeFile).then(json => {
                                self._cachedTimeseries[timeFile] = json;
                                return postRead(json);
                            });
                        }

                        // Pool promises
                        allPromises.push(promise);
                    }
                }
            }
        });
        return allPromises;
    }

    /**
     * Returns true if the input variable is a number.
     * 
     * @param {*} n 
     * @returns 
     */
    #isNumber(n) { 
        return !isNaN(parseFloat(n)) && !isNaN(n - 0);
    }

}
// End of class.