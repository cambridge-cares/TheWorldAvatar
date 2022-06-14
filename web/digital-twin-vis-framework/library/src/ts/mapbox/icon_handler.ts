
/**
 * Handles loading image icons for use on the Map.
 */
class IconHandler {

    /**
     * Load and cache the input image.
     * 
     * @param {String} imageURL relative image URL  
     */
    loadIcon(imageName, imageURL) {
        return new Promise<void>((resolve, reject) => {
            MapHandler.MAP.loadImage(
                imageURL,
                (error, image) => {
                    if(error) {
                        console.log("ERROR: Could not load image at URL " + imageURL);
                        reject(error);
                    } else {
                        this.addIcon(imageName, image);
                        resolve();
                    }
                }
            )
        });
    }

    /**
     * Load cached icons onto the map.
     */
    private addIcon(imageName, image) {    
        try {
            if(imageName.includes("-sdf")) {
                MapHandler.MAP.addImage(imageName, image, { 'sdf': true });
            } else {
                MapHandler.MAP.addImage(imageName, image);
            }
        } catch(error){
            console.trace();
        }
    }

}
// End of class.