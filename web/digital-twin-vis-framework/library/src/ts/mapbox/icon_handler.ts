
/**
 * Handles loading and registering image icons for use on Mapbox maps.
 */
class IconHandler {

    /**
     * Asynchronously loads and registers the input image.
     * 
     * @param {String} imageName name of image
     * @param {String} imageURL image URL  
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
                        // If the imageURL contains ("-sdf"), load as an SDF image
                        MapHandler.MAP.addImage(
                            imageName, 
                            image, 
                            { "sdf": imageURL.includes("-sdf") }
                        );
                        resolve(image);
                    }
                }
            );
        });
    }

}
// End of class.