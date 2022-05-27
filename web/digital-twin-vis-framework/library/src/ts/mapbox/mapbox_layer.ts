/**
 * Represents a single visual layer of data.
 */
class MapBoxLayer extends DataLayer {

    /**
     * Initialise a new MapBoxLayer instance.
     */
    constructor(id: string, name: string, source: DataSource) {
       super(id, name, source);
    }

    /**
     * Handles a click event on the input feature.
     * 
     * @param feature feature with layer that's been clicked on.
     */
    public handleClick(feature: Object) {
        console.log("Clicked on layer " + feature["layer"]["id"]);
        console.log(feature);
    }

    /**
     * Handles a mouse event on the input feature.
     * 
     * @param feature feature with layer that's been entered.
     */
    public handleMouseEnter(feature: Object) {
        if(feature !== null) {
            MapBoxUtils.showPopup(feature);
        }
    }

    /**
     * 
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