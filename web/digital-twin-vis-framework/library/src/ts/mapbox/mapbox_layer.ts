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

    /**
     * Returns true if layer is currently visible.
     */
    public isVisible(): boolean {
        let onMap = MapHandler.MAP.getLayer(this.id) !== undefined;
        if(onMap) {
            return MapHandler.MAP.getLayoutProperty(this.id, "visibility") === "visible";
        } else {
            return this.definition["layout"]["visibility"] === "visible";
        }
    }
}