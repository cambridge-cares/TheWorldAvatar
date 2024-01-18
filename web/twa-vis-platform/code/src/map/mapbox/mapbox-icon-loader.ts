import { IconSettings } from "../../types/map-settings";

/**
 * Adds icons to the map
 * 
 * @param iconSettings object listing icons for load.
 */
export function addIcons(iconSettings: IconSettings) {
    const promises: Promise<void>[] = [];

    for (const key of Object.keys(iconSettings)) {
        const promise = new Promise<void>(function(resolve, reject) {

            loadIcon(key, iconSettings[key], function() {
                resolve();
            });
        });
        promises.push(promise);
    }

    return Promise.all(promises).then(() => {
        console.info("All custom image icons have been loaded and registered.");
    });
}

/**
 * Asynchronously loads and registers the input image.
 * 
 * @param {String} imageName name of image
 * @param {String} imageURL image URL  
 * @param callback callback function
 */
function loadIcon(imageName: string, imageURL: string, callback: () => void) {

    window.map.loadImage(
        imageURL,
        (error, image) => {
            if(error) {
                console.log("ERROR: Could not load image at URL " + imageURL);
            } else {
                // If the imageURL contains ("-sdf"), load as an SDF image
                window.map.addImage(
                    imageName, 
                    image, 
                    { "sdf": imageURL.includes("-sdf") }
                );
            }
            callback();
        }
    );
}