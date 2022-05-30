/**
 * This class acts as an abstract handler for a map object provided by
 * a third party library.
 */
abstract class MapHandler {

    /**
     * API key for mapping libary (may be null).
     */
    public static MAP_API: String;

    /**
     * Main map object.
     */
    public static MAP;

    /**
     * Are click events currently processed
     */
    public static ALLOW_CLICKS: boolean = true;

    /**
     * Create and cache a new map object.
     */
    public abstract initialiseMap(mapOptions: Object);

    /**
     * Plot the contents of the input data group on the map.
     */
    public abstract plotData(dataStore: DataStore);

    /**
     * Creates a visual layer on the map based on the input layer definition.
     * 
     * @param group group containing the layer.
     * @param layer definition of layer to create.
     */
    public abstract plotLayer(group: DataGroup, layer: DataLayer);
}

/**
 * Possible map providing libraries.
 */
enum MapProvider {
    MAPBOX
}