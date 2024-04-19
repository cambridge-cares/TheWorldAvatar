/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataSource {

    /**
     * Unique name/id for this source.
     */
    public id: string;

    /**
     * Type of data.
     */
    public type: string;

    /**
     * The JSON object that originally defined this source.
     */
    public definition: Object;
   
    /**
     * Initialise a new DataSource instance based on a JSON object.
     * 
     * @param {Object} json JSON definition of the data source
     */
    constructor(json: Object) {
        this.validateJSON(json);

        this.definition = json;
        this.id = json["id"];
        this.type = json["type"];
    }    

    /**
     * Validates the JSON defining this data source.
     * 
     * @throws ReferenceError if required properties are missing.
     */
    private validateJSON(json: Object) {
        // Bug out if no definition
        if(json == null) throw new ReferenceError("'json' is not a non-null object.");

        // Must have a name
        if(!("id" in json)) throw new ReferenceError("Must contain a 'id' property.");

        // Must have a type
        if(!("type" in json)) throw new ReferenceError("Must contain a 'type' property.");
    }
}