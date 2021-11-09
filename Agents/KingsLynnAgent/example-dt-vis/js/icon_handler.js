
/**
 * Handles loading image icons for use on the Map.
 */
class IconHandler {

    // Mapbox map
    _map;

    /**
     * 
     * @param {*} map 
     */
    constructor(map) {
        this._map = map;
    }

    /**
     * 
     * @param {*} imageURL absolute image URL
     */
    registerIcon(imageURL) {
        var imageName = imageURL.replace(/^.*[\\\/]/, '');
        imageName = imageName.replace(/\.[^/.]+$/, "");

        this._map.loadImage(
            imageURL,
            (error, image) => {
                if(error) {
                    console.log("ERROR: Could not load image at URL " + imageURL);
                    throw error;
                } else {
                    this._map.addImage(imageName, image);
                    console.log("INFO: Image '" + imageName + "' has been added to the map.");
                }
            }
        )
}
}
// End of class.
