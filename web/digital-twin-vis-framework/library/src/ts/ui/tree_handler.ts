/**
 * Handles generation and logic for the tree of available groups and layers.
 */
class TreeHandler {
    
    /**
     * 
     */
    private registerEvents(dataStore: DataStore) {

        // Triggers when check events complete
        // @ts-ignore
        $("#treeview").on("CheckUncheckDone", function() {

            // Get all checked nodes
            var checkedItems = {"id" : [], "dataid" : [], "text" : []};

            // @ts-ignore
            $("#treeview").hummingbird("getChecked", {list: checkedItems, onlyEndNodes: true});
            
            // All Layer IDs
            let allLayers = DataUtils.getAllLayerIDs(dataStore);

            // Data IDs of all selected end nodes
            let visible = checkedItems["dataid"];
            let hidden = allLayers.filter(layer => !visible.includes(layer));

            visible.forEach(layer => MapBoxUtils.toggleLayer(layer, true));
            hidden.forEach(layer => MapBoxUtils.toggleLayer(layer, false));
        });
    }

    /**
     * 
     */
    public rebuild(dataStore: DataStore) {
        // // Reset the element
        let element = document.getElementById("treeview");
        element.innerHTML = "";

        // Rebuild recursively
        var preCheck = [];
        var htmlBuilder = [];

        for(var i = 0; i < dataStore.dataGroups.length; i++) { 
            this.buildRecurse(htmlBuilder, dataStore.dataGroups[i], 1, preCheck);
        }
        element.innerHTML += htmlBuilder.join("");

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

        this.registerEvents(dataStore);
    }


    private buildRecurse(htmlBuilder: string[], currentGroup: DataGroup, depth: number, preCheck: string[]) {
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

        // Layers within the group?
        for(var i = 0; i < currentGroup.dataLayers.length; i++) {
            let layer = currentGroup.dataLayers[i];

            let layerHTML = "<li class='end-node'>";
            layerHTML += "<label>"
            layerHTML += "<input class='hummingbird-end-node' id='" + layer.id + "' data-id='" + layer.id + "' type='checkbox'/><span>";
            layerHTML += layer.name;
            layerHTML += "</span></label>";
            layerHTML += "</li>";

            groupHTML += layerHTML;

            // Store if this needs to be prechecked
            if(layer instanceof MapBoxLayer && (<MapBoxLayer> layer).isVisible()) {
                preCheck.push(layer.id);
            }
        }

        // Append HTML
        htmlBuilder.push(groupHTML);

        // Recurse into sub groups?
        for(var i = 0; i < currentGroup.subGroups.length; i++) {
            let subNode = currentGroup.subGroups[i];
            this.buildRecurse(htmlBuilder, subNode, depth + 1, preCheck);
        }

        // Close
        if(needDeeper) {
            htmlBuilder.push("</ul>");
        }
        htmlBuilder.push("</li>");
    }




 
}