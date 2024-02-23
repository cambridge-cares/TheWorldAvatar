import { JsonObject } from "../../types/json";

/**
 * This class represents a single source of geographical data within the visualisation.
 */
export class DataSource {

    /**
     * Unique name/id for this source.
     */
    public readonly id: string;

    /**
     * Type of data.
     */
    public readonly type: string;

    /**
     * The JSON object that originally defined this source.
     */
    public readonly definition: JsonObject;
   
    /**
     * Initialise a new DataSource instance based on a JSON object.
     * 
     * @param id Unique name/id for this source.
     * @param type Type of data.
     * @param definition The JSON object that originally defined this source.
     */
    constructor(id: string, type: string, definition: object) {
        this.id = id;
        this.type = type;
        this.definition = (definition as JsonObject);

        console.info("Created DataSource instance '" + this.id + "'.");
    }    
}