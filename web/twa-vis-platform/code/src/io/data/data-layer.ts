import { JsonObject } from "../../types/json";
import { DataSource } from "./data-source";

/**
 * Represents a single visual layer of data. Will have concrete classes for each 
 * map provider type (i.e. MapboxDataLayer, CesiumDataLayer).
 */
export abstract class DataLayer {
    /**
     * Unique ID of layer.
     */
    public readonly id: string;

    /**
     * Public facing name of layer.
     */
    public readonly name: string;

    /**
     * Source of the layer's data.
     */
    public readonly source: DataSource;

    /**
     * The JSON object that originally defined this layer (unadjusted).
     */
    public readonly definition: JsonObject;

    /**
     * Zero-based display order.
     */
    public order: number = 0;

    /**
     * Type of interactions that are allowed ("all"|"hover-only"|"click-only"|"none")
     */
    public interactions: string = "all";

    /**
     * A cached visibility state that persists across map terrain
     * changes. Should be updated whenever visibility is changed
     * via the mapping API
     */
    public cachedVisibility: boolean = true;

    /**
     * Initialise a new DataLayer instance.
     * 
     * @param id Unique ID of layer.
     * @param name Public facing name of layer.
     * @param source Source of the layer's data.
     * @param definition The JSON object that originally defined this layer.
     */
    constructor(id: string, name: string, source: DataSource, definition: object) {
        this.id = id;
        this.name = name;
        this.source = source;
        this.definition = definition as JsonObject;

        console.info("Created DataLayer instance '" + this.id + "'.");
    }

}
// End of class.