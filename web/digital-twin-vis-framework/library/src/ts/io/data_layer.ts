/**
 * Represents a single visual layer of data.
 */
abstract class DataLayer {

    /**
     * Public facing name of layer.
     */
    public name: string;

    /**
     * Unique name of layer.
     */
    public id: string;

    /**
     * Source of the layer's data.
     */
    public source: DataSource;

    /**
     * The JSON object that originally defined this layer.
     */
    public definition: Object;

    /**
     * Initialise a new DataLayer instance.
     */
    constructor(id: string, name: string, source: DataSource) {
        this.id = id;
        this.name = name;
        this.source = source;

        console.info("Created DataLayer instance '" + this.name + "' with id '" + this.id + "'.");
    }

    /**
     * Returns true if this layer is interactable.
     * 
     * @returns true if clickable
     */
    public isClickable(): boolean {
        if(this.definition != null) {
            if("clickable" in Object.keys(this.definition)) {
                return this.definition["clickable"];
            }
        }
        return false;
    }

    /**
     * Handles a click event on the input feature.
     * 
     * @param feature feature with layer that's been clicked on.
     */
    public abstract handleClick(feature: Object);

    /**
     * Handles a mouse event on the input feature.
     * 
     * @param feature feature with layer that's been entered.
     */
    public abstract handleMouseEnter(feature: Object);

}