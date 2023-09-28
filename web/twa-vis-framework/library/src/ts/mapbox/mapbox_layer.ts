/**
 * Represents a single visual layer of data.
 */
class MapboxLayer extends DataLayer {

    /**
     * Initialise a new MapboxLayer instance.
     */
    constructor(id: string, name: string, source: DataSource) {
       super(id, name, source);
    }

}
// End of class.