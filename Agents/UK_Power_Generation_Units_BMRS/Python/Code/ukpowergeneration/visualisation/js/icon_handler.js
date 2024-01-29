
/**
 * Handles loading image icons for use on the Map.
 */
class IconHandler {

    // Mapbox map
    _map;

    // Cached images
    _loadedImages = [];

    /**
     * Initialise new IconHandler.
     * 
     * @param {*} map 
     */
    constructor(map) {
        this._map = map;
    }

    /**
     * Load and cache the input image.
     * 
     * @param {String} imageURL relative image URL  
     */
    loadIcon(imageURL) {
        var imageName = imageURL.replace(/^.*[\\\/]/, '');
        imageName = imageName.replace(/\.[^/.]+$/, "");

        return new Promise((resolve, reject) => {
            this._map.loadImage(
                imageURL,
                (error, image) => {
                    if(error) {
                        console.log("ERROR: Could not load image at URL " + imageURL);
                        reject(error);
                    } else {
                        this._loadedImages[imageName] = image;
                        resolve();
                    }
                }
            )

        });
    }

    /**
     * Load cached icons onto the map.
     */
    addAllIcons() {    
        for(const [key, value] of Object.entries(this._loadedImages)) {
            var imageName = key.replace(/^.*[\\\/]/, '');
            imageName = imageName.replace(/\.[^/.]+$/, "");
            
            try {
                if(key.includes("-sdf")) {
                    this._map.addImage(imageName, value, { 'sdf': true });
                } else {
                    this._map.addImage(imageName, value);
                }
            } catch(error){
                console.log(imageName);
                console.trace();
            }
        }
    }

}
// End of class.