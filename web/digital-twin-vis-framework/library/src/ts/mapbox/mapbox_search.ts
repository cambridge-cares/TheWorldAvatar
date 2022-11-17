/**
 * Concrete implementation of the SearchHandler class that handles
 * executing searchs as filters on Mapbox maps.
 */
class SearchHandler_Mapbox extends SearchHandler {

    /**
     * Any filters previously used on layers, stored by layer ID.
     */
    private previousFilters = {};

    /**
     * Caches mapbox sources so that clustering can be renabled.
     */
    private cachedSources = {};


    /**
     * When applicable, turn off clustering for each source currently
     * loaded within the map's style object.
     */
    public turnOffClustering() {
        this.cachedSources = {};
        const style = MapHandler.MAP.getStyle();
        const sources = style.sources;

        Object.entries(sources).forEach(([name, source]) => {
            // Is this source clustered?
            if(source.hasOwnProperty("cluster") && source["cluster"]) {
                // Cache the source
                this.cachedSources[name] = source;
                
                // Turn off clustering
                source["cluster"] = false;
            }
        });
    
        // Re-apply the style to force update
        MapHandler.MAP.setStyle(style);
    }

    /**
     * Using cached knowledge of what sources were clustered,
     * reset each back to their original setting.
     */
    public turnOnClustering() {
        const style = MapHandler.MAP.getStyle();
        const sources = style.sources;

        Object.entries(sources).forEach(([name, source]) => {
            // Is clustering defined but false?
            if(source.hasOwnProperty("cluster") && source["cluster"] === false) {
                // Was this source previously clustered?
                if(this.cachedSources.hasOwnProperty(name)) {
                    // Turn clustering back on
                    source["cluster"] = true;
                }
            }
        });
        this.cachedSources = {};

        // Re-apply the style to force update
        MapHandler.MAP.setStyle(style);
    }

    /**
     * Execute the filter with the input search term.
     * 
     * @param searchTerm search term
     */
    public runSearch(searchTerm: string | number | boolean) {
        // Cancel previous search
        this.cancelSearch();
        let layers = MapHandler.MAP.getStyle().layers;

        for(let i = 0; i < layers.length; i++) {
            // Skip if a layer not provided by the DTVF
            let meta = layers[i]["metadata"];
            if(meta === null || meta === undefined) continue;
            let attr = meta["attribution"];
            if(attr === null || attr === undefined || attr !== "CMCL Innovations") continue;
            
            let id = layers[i]["id"];
            let oldFilter = MapHandler.MAP.getFilter(id)
        
            // Apply new filter
            let filter = this.createFilter(oldFilter, searchTerm);
            if(filter !== null && filter !== undefined) {
                this.previousFilters[id] = oldFilter;
                MapHandler.MAP.setFilter(id, filter);
            }
        }
    }

    /**
     * Clear the filter.
     */
    public cancelSearch() {
        if(this.previousFilters == null) return;

        // Revert to previously cached filters
        Object.entries(this.previousFilters).forEach(([layerID, filter]) => {
            MapHandler.MAP.setFilter(layerID, filter);
        });

        // Clear cached filters
        this.previousFilters = {};
    }

    /**
     * Create the filter object based on the current search settings.
     */
    private createFilter(oldFilter, searchTerm): Object {
        let propName = this.property["property"];

        // Filter object
        let filter = null;

        // Determine range selection
        let rangeSelect = document.getElementById("finderRangeSelect") as HTMLInputElement;

         // Build filter based on type and range
        if(typeof searchTerm === "boolean") {
            // Is a boolean
            switch(rangeSelect.value) {
                case "true": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["==", ["to-boolean", ["get", propName]], true]
                    ];
                }
                break;
                case "false": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["==", ["to-boolean", ["get", propName]], false]
                    ];
                }
                break;
            }
        } else if(!isNaN(searchTerm)) {
            // Is a number
            switch(rangeSelect.value) {
                case "equals": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["==", Number(searchTerm), ["to-number", ["get", propName]]]
                    ];
                }
                break;
                case "lesser": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["<", ["to-number", ["get", propName]], Number(searchTerm)]
                    ];
                }
                break;
                case "greater": {
                    filter = [
                        "all",
                        ["has", propName],
                        [">", ["to-number", ["get", propName]], Number(searchTerm)]
                    ];
                }
                break;
            }

        } else if(String(searchTerm)) {
            // Is a string
            searchTerm = searchTerm.toLowerCase();

            switch(rangeSelect.value) {
                case "contains": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["in", searchTerm, ["downcase", ["get", propName]]]
                    ];
                }
                break;

                case "starts": {
                    filter = [
                        "all",
                        ["has", propName],
                        ["==", ["index-of", searchTerm, ["downcase", ["get", propName]]], 0]
                    ];
                }
                break;
            }
        }  
        if(filter === null || filter === undefined) return null;

        // If a filter already exists, and it
        if(oldFilter !== null && oldFilter !== undefined) {
            filter = [
                "all",
                oldFilter,
                filter
            ];
        }
        return filter;
    }

}