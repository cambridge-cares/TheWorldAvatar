import { Map } from 'mapbox-gl';
import { IconSettings } from 'types/settings';

/**
 * Adds icons to the map
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {IconSettings} iconSettings object listing icons for load.
 */
export async function addIcons(map: Map, iconSettings: IconSettings): Promise<void> {
  if (!iconSettings) return;

  const promises = Object.keys(iconSettings).map(async (key) => {
    return new Promise<void>((resolve) => {
      loadIcon(map, key, iconSettings[key], () => resolve());
    });
  });

  await Promise.all(promises);
  console.info("All custom image icons have been loaded and registered.");
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