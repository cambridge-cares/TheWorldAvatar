
/**
 * Handles loading image icons for use on the Map.
 */
class IconHandler {

    /**
     * Load and cache the input image.
     * 
     * @param {String} imageURL relative image URL  
     */
    public async loadIcon(imageName, imageURL) {
        await new Promise<void>((resolve, reject) => {
            MapHandler.MAP.loadImage(
                imageURL,
                (error, image) => {
                    if(error) {
                        console.log("ERROR: Could not load image at URL " + imageURL);
                        reject(error);
                    } else {
                        MapHandler.MAP.addImage(imageName, image);
                        resolve(image);
                    }
                }
            );
        });
    }

}
// End of class.