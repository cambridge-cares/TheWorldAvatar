import { JsonObject } from 'types/json';

/**
 * This class represents a single source of geographical data within the visualisation.
 */
export class LayerSource {

    /**
     * Unique name/id for this source.
     */
    public readonly id: string;

    /**
     * Type of data.
     */
    public readonly type: string;

    /**
     * The stack endpoint that stores the associated data to the geographical data.
     */
    public readonly stackEndpoint: string;

    /**
     * The JSON object that originally defined this source.
     */
    public readonly definition: JsonObject;
   
    /**
     * Initialise a new LayerSource instance based on a JSON object.
     * 
     * @param id Unique name/id for this source.
     * @param type Type of data.
     * @param stackEndpoint The stack endpoint storing the data.
     * @param definition The JSON object that originally defined this source.
     */
    constructor(id: string, type: string, stackEndpoint: string, definition: object) {
        this.id = id;
        this.type = type;
        this.definition = (definition as JsonObject);
        this.stackEndpoint = stackEndpoint;

        console.info("Created LayerSource instance '" + this.id + "'.");
    }    
}