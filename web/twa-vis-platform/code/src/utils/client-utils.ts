/**
 * Utilities to be run on the client.
 */

/**
 * Open full screen mode.
 */
export function openFullscreen() {
    const elem = document?.documentElement;
    if(elem?.requestFullscreen) {
        elem.requestFullscreen();
    } 
}
  
/**
 * Close fullscreen mode.
 */
export function closeFullscreen() {
    if(document?.exitFullscreen) {
        document.exitFullscreen();
    }
}

/**
 * Query the server to get the map-settings.json file.
 */
export async function getMapSettings() {
    return fetch("/api/visualisation/settings", { cache: "force-cache" })
            .then((result) => result.json())
            .catch((err) => console.log(err));
}