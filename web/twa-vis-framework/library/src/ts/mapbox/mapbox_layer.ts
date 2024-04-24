/**
 * Represents a single visual layer of data.
 */
class MapboxLayer extends DataLayer {

    /**
     * Cache of original values of injectable properties (using [HOVERED-IRI]).
     */
    public injectableHoverProps = {};

    /**
     * Cache of original values of injectable properties (using [SELECTED-IRI]).
     */
    public injectableSelectProps = {};

    /**
     * Initialise a new MapboxLayer instance.
     */
    constructor(id: string, name: string, source: DataSource) {
       super(id, name, source);
    }

    /**
     * Returns a map of properties (key, value) that have been cached as
     * injectable with hoverable or selectable IRIs.
     * 
     * @param interactionType interaction type ("hover", "select").
     * @param propertyType property type ("paint", "layout", "filter").
     * 
     * @returns map of properties (key, value). 
     */
    public getCachedProperties(interactionType, propertyType) {
        if(interactionType === "hover") {
            return this.injectableHoverProps[propertyType];
        } else if(interactionType === "select") {
            return this.injectableSelectProps[propertyType];
        }
    }

    /**
     * Search through the layer's definition and cache the presence of any paint, layout, or filter
     * that has the placeholders for injectable IRI values.
     */
    public cacheInjectableProperties() {
        // Store container objects
        this.injectableHoverProps["paint"] = {};
        this.injectableHoverProps["layout"] = {};
        this.injectableSelectProps["paint"] = {};
        this.injectableSelectProps["layout"] = {};

        // Iterate through paint properties
        let paint = this.definition["paint"];
        if(paint != null) {
            for (const [key, value] of Object.entries(paint)) {
                
                if(JSON.stringify(value).includes("[HOVERED-IRI]")) {
                    // Cache as a hoverable property
                    this.injectableHoverProps["paint"][key] = value;
                } else if(JSON.stringify(value).includes("[SELECTED-IRI]")) {
                    // Cache as a selectable property
                    this.injectableSelectProps["paint"][key] = value;
                }
            }
        }

        // Iterate through layout properties
        let layout = this.definition["layout"];
        if(layout != null) {
            for (const [key, value] of Object.entries(layout)) {
                
                if(JSON.stringify(value).includes("[HOVERED-IRI]")) {
                    // Cache as a hoverable property
                    this.injectableHoverProps["layout"][key] = value;
                } else if(JSON.stringify(value).includes("[SELECTED-IRI]")) {
                    // Cache as a selectable property
                    this.injectableSelectProps["layout"][key] = value;
                }
            }
        }

        // Check the filter
        let filter = this.definition["filter"];
        if(filter != null) {

            if(JSON.stringify(filter).includes("[HOVERED-IRI")) {
                // Cache as a hoverable property
                this.injectableHoverProps["filter"] = filter;
            } else if(JSON.stringify(filter).includes("[SELECTED-IRI]")) {
                // Cache as a selectable property
                this.injectableSelectProps["filter"] = filter;
            }
        }
    }
}