import { Map } from 'mapbox-gl';
import { IconSettings } from 'types/settings';

/**
 * Adds icons to the map
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {IconSettings} iconSettings object listing icons for load.
 */
export function addIcons(map: Map, iconSettings: IconSettings) {
  const promises: Promise<void>[] = [];
  if (iconSettings) {
    for (const key of Object.keys(iconSettings)) {
      const promise = new Promise<void>(function (resolve) {
        loadIcon(map, key, iconSettings[key], function () {
          resolve();
        });
      });
      promises.push(promise);
    }
  }
  return Promise.all(promises).then(() => {
    console.info("All custom image icons have been loaded and registered.");
  });
}

/**
 * Asynchronously loads and registers the input image.
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {String} imageName name of image
 * @param {String} imageURL image URL  
 * @param callback callback function
 */
function loadIcon(map: Map, imageName: string, imageURL: string, callback: () => void) {
  map?.loadImage(
    imageURL,
    (error, image) => {
      if (error) {
        console.log("ERROR: Could not load image at URL " + imageURL);
      } else {
        if (!map?.hasImage(imageName)) {
          // If the imageURL contains ("-sdf"), load as an SDF image
          map?.addImage(
            imageName,
            image,
            { "sdf": imageURL.includes("-sdf") }
          );
        }
      }
      callback();
    }
  );
}