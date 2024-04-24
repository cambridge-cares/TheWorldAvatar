
/**
 * Handles loading and registering image icons for use on Mapbox maps.
 */
class IconHandler {

    /**
     * Asynchronously loads and registers the input image.
     * 
     * @param {String} imageName name of image
     * @param {String} imageURL image URL  
     * @param callback callback function
     */
     public loadIcon(imageName, imageURL, callback) {

        MapHandler.MAP.loadImage(
            imageURL,
            (error, image) => {
                if(error) {
                    console.log("ERROR: Could not load image at URL " + imageURL);
                } else {
                    // If the imageURL contains ("-sdf"), load as an SDF image
                    MapHandler.MAP.addImage(
                        imageName, 
                        image, 
                        { "sdf": imageURL.includes("-sdf") }
                    );
                }
                callback();
            }
        );
    }

}
// End of class.