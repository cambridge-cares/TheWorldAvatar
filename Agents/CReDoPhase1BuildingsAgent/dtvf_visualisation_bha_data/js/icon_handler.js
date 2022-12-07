
/**
 * Handles loading image icons for use on the Map.
 */
class IconHandler {

    // Mapbox map
    _map;

    //
    _loadedImages = [];

    /**
     * 
     * @param {*} map 
     */
    constructor(map) {
        this._map = map;
    }

    /**
     * 
     * @param {*} imageURL 
     * @param {*} callback 
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
                        console.log("INFO: Image '" + imageName + "' has been loaded.");
                        resolve();
                    }
                }
            )

        });
    }

    /**
     * 
     */
    addAllIcons() {    
        for(const [key, value] of Object.entries(this._loadedImages)) {
            var imageName = key.replace(/^.*[\\\/]/, '');
            imageName = imageName.replace(/\.[^/.]+$/, "");
            
            if(key.includes("-sdf")) {
                this._map.addImage(imageName, value, { 'sdf': true });
            } else {
                this._map.addImage(imageName, value);
            }
            console.log("INFO: Image '" + imageName + "' has been added.");
        }
    }
}
// End of class.
