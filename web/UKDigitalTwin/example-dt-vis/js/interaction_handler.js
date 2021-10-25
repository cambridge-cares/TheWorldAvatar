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

    _previousTab;

    /**
     * Initialise a new interaction handler.
     * 
     * @param {*} map 
     * @param {*} dataRegistry 
     */
    constructor(map, dataRegistry, panelHandler) {
        this._map = map;
        this._dataRegistry = dataRegistry;
        this._panelHandler = panelHandler;

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
            this.mouseClick(layerName, feature);
        });

        console.log("INFO: Interactions for layer '" + layerName + "' have been registered.");
    }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     */
    mouseClick(layerName, feature) {
        // Clear existing side panel content
        this._panelHandler.setContent("");

        // Hide the legend
        this._panelHandler.toggleLegend(false);
        
        // Show the title
        var title = feature.properties["displayName"];
        if(title == null) feature.properties["name"];
        this._panelHandler.setTitle(title);

        // Build containers for metadata trees
        this.#buildMetadataContainers();

        // Add the fixed data
        var fixedTree = JsonView.renderJSON(feature.properties, document.getElementById("fixed-tree"));
        JsonView.expandChildren(fixedTree);

        if(DT.currentAdditionals.length > 0) {
            // Find additional metadata for the currently selected additional group
            this.#findAdditionalMeta(layerName, feature, function(additionalMeta) {
                var additionalTree = JsonView.renderJSON(additionalMeta, document.getElementById("additional-tree"));
                JsonView.expandChildren(additionalTree);
            });
        } else {
            document.getElementById("additional-tree").innerHTML = this.#NO_ADDITIONAL;
        }

        // Restore previously open tab 
        if(this._previousTab != null) {
            this.openTreeTab(this._previousTab + "-button", this._previousTab);
        } else {
            this.openTreeTab("fixed-tree-button", "fixed-tree");
        }

        // Remember the current feature
        DT.currentFeature = feature;
    }

    #buildMetadataContainers() {
        var html = `<div class="json-tree-tabs">`;

        if(this._dataRegistry.additionalMeta == null) {
            html += `
                <button id="fixed-tree-button" class="tablinks" onclick="manager.openTreeTab(this.id, 'fixed-tree')">Fixed Metadata</button>
            `;
        } else {
            html += `
                <button id="fixed-tree-button" class="tablinks" onclick="manager.openTreeTab(this.id, 'fixed-tree')">Fixed Metadata</button>
                <button id="additional-tree-button" class="tablinks" onclick="manager.openTreeTab(this.id, 'additional-tree')">Additional Metadata</button>
            `;
        }

        html += `
            </div>
            <div id="fixed-tree" style="margin-top: 10px;" class="tabcontent"></div>
            <div id="additional-tree" style="margin-top: 10px;" class="tabcontent"></div>
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

        this._previousTab = tabName;
      }

    /**
     * 
     * @param {*} layerName 
     * @param {*} feature 
     */
    #findAdditionalMeta(layerName, feature, callback) {
        var resultingMeta = {};

        DT.currentAdditionals.forEach(groupListing => {
            let metaDir = this._dataRegistry.getAdditionalDirectory(groupListing);
            let metaGroup = this._dataRegistry.getAdditionalGroup(groupListing);
            if(metaDir == null || metaGroup == null) return;
           
            metaGroup["dataSets"].forEach(dataSet => {
                if(dataSet["name"] === layerName) {

                    // JSON Metadata
                    if(dataSet["metaFiles"]) {
                        let metaFiles = dataSet["metaFiles"];
                        let metaPromises = [];

                        metaFiles.forEach(metaFilename => {
                            let metaFile = metaDir + "/" + metaFilename;
                            console.log("INFO: Reading Additional metadata JSON at " + metaFile);

                            metaPromises.push($.getJSON(metaFile, function(json) {
                                return json;
                            }));
                        });

                        // After all JSON are loaded
                        Promise.all(metaPromises).then((jsons) => {
                            jsons.forEach(json => {

                                for(var i = 0; i < json.length; i++) {
                                    var entry = json[i];

                                    if(entry["id"] == feature.id) {
                                        Object.keys(entry).forEach(function(key){
                                            resultingMeta[key] = entry[key];
                                        });
                                    }
                                }
                            });

                            if(callback != null) {
                                callback(resultingMeta);
                            }
                        });
                    }
                }
            });
        });
    }


}
// End of class.