/**
 * Represents a single visual layer of data.
 */
class MapBoxLayer extends DataLayer {

    /**
     * Initialise a new MapBoxLayer instance.
     */
    constructor(name: String, source: DataSource) {
       super(name, source);
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
}