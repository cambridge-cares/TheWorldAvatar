/**
 * Represents a single visual layer of data.
 */
abstract class DataLayer {

    /**
     * Public facing name of layer.
     */
    public name: string;

    /**
     * Unique ID of layer.
     */
    public id: string;

    /**
     * Source of the layer's data.
     */
    public source: DataSource;

    /**
     * The JSON object that originally defined this layer.
     */
    public definition: Object;

    /**
     * Zero-based display order.
     */
    public order: number = 0;

    /**
     * Type of interactions that are allowed ("all"|"hover-only"|"click-only"|"none")
     */
    public interactions = "all";

    /**
     * A cached visibility state that persists across map terrain
     * changes. Should be updated whenever visibility is changed
     * via the mapping API
     */
    private isVisible = true;

    /**
     * Initialise a new DataLayer instance.
     */
    constructor(id: string, name: string, source: DataSource) {
        this.id = id;
        this.name = name;
        this.source = source;
    }

    /**
     * Cache the current visibility state.
     * 
     * @param isVisible current visibility state
     */
    public cacheVisibility(isVisible) {
        this.isVisible = isVisible;
    }

    /**
     * Returns the cached visibility state.
     * 
     * @returns boolean of cached visibility
     */
    public getVisibility() {
        return this.isVisible;
    }

}
// End of class.