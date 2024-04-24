/**
 * Handles generation and logic for the tree of available groups and layers.
 */
class TreeHandler {
    
    /**
     * Optional callbacks that fire with two lists of layerIDs (visible, hidden)
     * when any change in the tree's selection state takes place.
     */
    private treeSelectionCallbacks = [];

    /**
     * Adds a callback that will fire fire with two lists of layerIDs (visible, hidden)
     * when any change in the tree's selection state takes place.
     * 
     * @param treeSelectionCallback callback function.
     */
    public addTreeSelectionCallback(treeSelectionCallback) {
        this.treeSelectionCallbacks.push(treeSelectionCallback);
    }

    /**
     * Triggers when any selection states in the tree change.
     */
    private registerEvents(dataStore: DataStore) {
        // Triggers when check events complete
        let self  = this;
        $("#treeview").on("CheckUncheckDone", function() {

            // Get all checked nodes
            var checkedItems = {"id" : [], "dataid" : [], "text" : []};

            // @ts-ignore
            $("#treeview").hummingbird("getChecked", {list: checkedItems, onlyEndNodes: true});
            
            // Data IDs of all selected end nodes
            let checked = checkedItems["dataid"];

            // Parse to visible layer IDs
            let visible = [];
            checked.forEach(id => {
                if(id.includes("|")) {
                    id.split("|").forEach(part => visible.push(part));
                } else {
                    visible.push(id);
                }
            });

            // Layers not in visible array are hidden
            let allLayers = DataUtils.getAllLayerIDs(dataStore);
            let hidden = allLayers.filter(layer => !visible.includes(layer));

            // Update visibility
            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    visible.forEach(layer => MapboxUtils.toggleLayer(layer, true));
                    hidden.forEach(layer => MapboxUtils.toggleLayer(layer, false));
                break;

                case MapProvider.CESIUM:
                    visible.forEach(layer => CesiumUtils.toggleLayer(layer, true));
                    hidden.forEach(layer => CesiumUtils.toggleLayer(layer, false));
                break;
            }

            // Fire state change callbacks
            self.treeSelectionCallbacks.forEach(callback => {
                callback(visible, hidden);
            });
        });
    }

    /**
     * Rebuilds the tree based on the input DataStore instance.
     */
    public rebuild(dataStore: DataStore) {
        // Reset the element
        let element = document.getElementById("treeview");
        element.innerHTML = "";

        // Rebuild recursively
        var preCheck = [];
        var collapse = [];
        var htmlBuilder = [];

        for(var i = 0; i < dataStore.dataGroups.length; i++) { 
            this.buildRecurse(htmlBuilder, dataStore.dataGroups[i], 1, preCheck, collapse);
        }
        element.innerHTML += htmlBuilder.join("");

        // @ts-ignore
        $.fn.hummingbird.defaults.SymbolPrefix = "fas";
        // @ts-ignore
        $.fn.hummingbird.defaults.collapsedSymbol = "fa-chevron-down";
        // @ts-ignore
        $.fn.hummingbird.defaults.expandedSymbol = "fa-chevron-up";
        // @ts-ignore
        $("#treeview").hummingbird();

        // Precheck visible layers
        // @ts-ignore
        $("#treeview").hummingbird("checkNode", {sel: "data-id", vals: preCheck});

        // Expand by default
        // @ts-ignore
        $("#treeview").hummingbird("expandAll");

        // Collapse some specific layers
        // @ts-ignore
        $("#treeview").hummingbird("collapseNode", {sel: "data-id", vals: collapse});

        this.registerEvents(dataStore);
        console.log("Finished rebuilding layer tree.");
    }


    /**
     * Recursively build HTML elements.
     */
    private buildRecurse(htmlBuilder: string[], currentGroup: DataGroup, depth: number, preCheck: string[], collapse: string[]) {
        // Entry for group itself
        let groupHTML = "<li data-id='" + (depth - 1) + "'>";
        groupHTML += "<i class='fa fa-plus'></i>";
        groupHTML += "<label>"
        
        groupHTML += "<input id='" + currentGroup.id + "' data-id='" + currentGroup.id + "' type='checkbox'/>";
        groupHTML += currentGroup.name;
        groupHTML += "</label>";

        // Need to progress deeper into the tree?
        let needDeeper = (currentGroup.dataLayers.length > 0 || currentGroup.subGroups.length > 0);
        if(needDeeper) groupHTML += "<ul>";

        // Store if this needs to be collapsed by default
        if(!currentGroup.defaultExpanded) {
            collapse.push(currentGroup.id);
        }

        // Sort layers in the group via name (there may be duplicates)
        let sortedLayers = {};
        currentGroup.dataLayers.forEach(layer => {
            if(sortedLayers[layer.name] === null || sortedLayers[layer.name] === undefined) {
                sortedLayers[layer.name] = [];
            }
            sortedLayers[layer.name].push(layer);
        });

        for(const [key, value] of Object.entries(sortedLayers)) {

            // Build dataID
            let layers = value as Array<DataLayer>;
            let dataID = "";

            // Are ALL layers here marked as un-treeable?
            let allHidden = true;
            layers.forEach(layer => {
                if(!layer.definition.hasOwnProperty("treeable") || layer.definition["treeable"] === true) {
                    allHidden = false;
                }

                dataID += layer.id + "|";
            });
        
            // Clean up
            if(dataID.endsWith("|")) dataID = dataID.slice(0, -1);

            // Build HTML
            let layerHTML = "<li class='end-node'";

            // If all layers are marked as un-treeable, add it to the tree but make it invisible. This is required as
            // we use the structure of the tree later to determine checked and unchecked layers.
            if(allHidden) {
                layerHTML += "style='display:none;'>";
            } else {
                layerHTML += ">"
            }

            // Continue building HTML
            layerHTML += "<label>"
            layerHTML += "<input class='hummingbird-end-node' id='" + dataID + "' data-id='" + dataID + "' type='checkbox'/><span>";
            layerHTML += key;
            layerHTML += "</span></label>";
            layerHTML += "</li>";
            groupHTML += layerHTML;

            // Store if this needs to be prechecked
            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    let mbLayer = <MapboxLayer> value[0];
                    if(mbLayer.getVisibility()) preCheck.push(dataID);
                break;

                case MapProvider.CESIUM:
                    let csLayer = <CesiumLayer> value[0];
                    let dataSources = MapHandler_Cesium.DATA_SOURCES[csLayer.id];

                    if(dataSources == null || dataSources.length === 0) {
                        let visibility = csLayer.definition["visibility"];
                        if(visibility == null && csLayer.definition.hasOwnProperty("layout")) {
                            visibility = csLayer.definition["layout"]["visibility"];
                        }
            
                        if(visibility == null || visibility === "visible") preCheck.push(dataID);
                    } else {
                        if(csLayer.getVisibility()) preCheck.push(dataID);
                    }
                break;
            }
        }

        // Append HTML
        htmlBuilder.push(groupHTML);

        // Recurse into sub groups?
        for(var i = 0; i < currentGroup.subGroups.length; i++) {
            let subNode = currentGroup.subGroups[i];
            this.buildRecurse(htmlBuilder, subNode, depth + 1, preCheck, collapse);
        }

        // Close
        if(needDeeper) {
            htmlBuilder.push("</ul>");
        }
        htmlBuilder.push("</li>");
    }
}