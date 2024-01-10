/**
 * Utilities methods related to camera operations on Mapbox maps.
 */

import "react-toastify/dist/ReactToastify.css";
import { ToastContainer, toast } from "react-toastify";

import { CameraPosition, CameraSettings, MapSettings } from "types/map-settings";
import { getMapSettings } from "../../utils/client-utils";
import { reduxStore } from "../../app/store";

/**
 * Returns the default camera position object.
 * 
 * @param mapSettings MapSettings object.
 * 
 * @returns default CameraPosition object.
 */
export function getDefaultCameraPosition(mapSettings: MapSettings): CameraPosition {
    const cameraSettings = mapSettings.camera
    const defaultName = cameraSettings.default;
    return getCameraPosition(defaultName, cameraSettings);
}

/**
 * Returns the camera position object that matches the input name.
 * 
 * @param name Camera position name
 * @param cameraSettings Camera settings object
 * @returns Corresponding CameraPosition (or null)
 */
export function getCameraPosition(name: string, cameraSettings: CameraSettings): CameraPosition {
    const positions = cameraSettings.positions;
    return positions.find(item => item["name"] === name);
}

/**
 * Toggle's 3D terrain.
 * 
 * @param state toggle flag
 */
export function set3DTerrain(state: boolean) {
    const map = window.map;

    if(state) {
        map.addSource('mapbox-3d-terrain', {
            type: 'raster-dem',
            url: 'mapbox://mapbox.mapbox-terrain-dem-v1',
            tileSize: 512,
            maxzoom: 14
        });
        map.setTerrain({
            source: 'mapbox-3d-terrain', 
            exaggeration: 1.5
        });
    } else {
        map.setTerrain(null);
        map.removeSource('mapbox-3d-terrain');
    }

}

/**
 * Reset the camera to the current position (as defined in Redux state).
 */
export async function resetCamera() {
    const mapSettings = await getMapSettings();
    const reduxState = reduxStore.getState();
    const items = reduxState.ribbonComponents.items;

    let position: CameraPosition;
    if(items.length == 0) {
        position = getDefaultCameraPosition(mapSettings);
    } else {
        const positionName = items.find(position => position.name === "Reset Camera")?.selection;
        position = getCameraPosition(positionName, mapSettings.camera);
    }

    // Move the map
    window.map.flyTo({
        ...position,
        essential: true
    });
}

/**
 * If given permission by browser alert, this moves the map to the user's location.
 */
export async function locateUser() {
    navigator.geolocation.getCurrentPosition(
        (geolocation) => {
            const long = geolocation["coords"]["longitude"];
            const lat = geolocation["coords"]["latitude"];

            // Move the map
            window.map.flyTo({
                center: [long, lat],
                zoom: 12,
                essential: true
            });
        },
        (err) => {
            toast.warning(
                "Cannot read user's location without browser authorisation.",
                { position: toast.POSITION.BOTTOM_LEFT }
            )
        }
    );
}