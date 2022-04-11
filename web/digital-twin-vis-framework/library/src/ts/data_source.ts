/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataSource {

    /**
     * Unique name/id for this source.
     */
    private name: string;

    /**
     * The type of data this source represents.
     */
    private type: DataType;

    /**
     * The JSON object that originally defined this source.
     */
    private definition: Object;

    /**
     * Object containing the real data, may be null.
     */
    private data: Object;

    /**
     * Initialise a new DataSource instance based on a JSON object.
     * 
     * @param {Object} json JSOn definition of the data source
     */
    constructor(json: Object) {
        // Bug out if no definition
        if(json == null) throw new TypeError("'json' is not a non-null object.");
        
        // Required properties
        if(!("name" in json)) throw new ReferenceError("'json' must contain a 'name' property.");
        if(!("type" in json))  throw new ReferenceError("'json' must contain a 'type' property.");

        // Check for value enum
        if(!(json["type"].toUpperCase() in Object.keys(DataType))) {
            throw new TypeError("'json' must contain a 'type' property with a value supported by the 'DataType' enum.");
        }

        this.#definition = json;
        this.#name = json["name"];
    }

    
}


/**
 * This class acts as an enumerator for the types of geographic data supported
 * by the framework.
 */
 enum DataType {
    POINT    = "point",
    LINE     = "line",
    POLYGON  = "polygon",
    RASTER   = "raster",
    VECTOR   = "vector"
}