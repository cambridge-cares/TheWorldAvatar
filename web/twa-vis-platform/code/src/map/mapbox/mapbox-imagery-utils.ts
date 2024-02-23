/**
 * Utilities methods related to base map imagery options on Mapbox maps.
 */

import { ImageryOption, ImagerySettings, MapSettings } from "types/map-settings";
import { getMapSettings } from "../../utils/client-utils";
import { reduxStore } from "../../app/store";

/**
 * Returns the default imagery option.
 * 
 * @param mapSettings MapSettings object.
 * 
 * @returns default ImageryOption object.
 */
export function getDefaultImageryOption(mapSettings: MapSettings): ImageryOption {
    let imageryOptions = mapSettings.imagery;
    if(imageryOptions == null) {
        imageryOptions = defaultImageryOptions;
    }

    if(mapSettings.imagery.default.toLowerCase() == "auto") {
        // Auto detect browser theme
        if (window?.matchMedia && window?.matchMedia('(prefers-color-scheme: dark)').matches) {
            return getImageryOption("3D (Night)", mapSettings.imagery);
        } else {
            return getImageryOption("3D (Day)", mapSettings.imagery);
        }
    } else {
        return getImageryOption(imageryOptions.default, imageryOptions);
    }
}

/**
 * Returns the imagery option that matches the input name.
 * 
 * @param name Imagery option name
 * @param cameraSettings Imagery settings object
 * @returns Corresponding ImageryOption (or null)
 */
export function getImageryOption(name: string, imageryOptions: ImagerySettings): ImageryOption {
    if(imageryOptions == null) {
        imageryOptions = defaultImageryOptions;
    }

    const options = imageryOptions.options;
    return options.find(item => item["name"] === name);
}

/**
 * Returns the currently selected ImageryOption object.
 * 
 * @returns ImageryOption for current selection.
 */
export function getCurrentImageryOption(mapSettings: MapSettings) {
    const reduxState = reduxStore.getState();
    const items = reduxState.ribbonComponents.items;
    
    if(items == null || items.length == 0) {
        return getDefaultImageryOption(mapSettings);
    } else {
        const match = items.find(option => option.name === "Imagery");
        if (match == null) {
            return getDefaultImageryOption(mapSettings);
        } else {
            return getImageryOption(match.selection, mapSettings.imagery);
        }
    }
}

/**
 * Set the base map imagery to the current option (as defined in Redux state).
 */
export async function setImagery() {
    const mapSettings = await getMapSettings();
    const imageryOption: ImageryOption = getCurrentImageryOption(mapSettings);
    
    // Update map
    window.map.setStyle(imageryOption.url);
    window.map.setProjection({
        name: 'mercator'
    });

    window.map.on('style.load', () => {
        if(imageryOption.time != null) {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            (window.map as any).setConfigProperty('basemap', 'lightPreset', imageryOption.time);
        }

        // Ensure placenames match previous state
        togglePlacenames();
    });
}

/**
 * Toggle the display of all placenames on the map.
 */
export async function togglePlacenames() {
    const mapSettings = await getMapSettings();
    const reduxState = reduxStore.getState();
    const items = reduxState.ribbonComponents.items;

    let shouldHide = true;
    if(items != null && items.length > 0) {
        shouldHide = items.find(option => option.name === "Hide Labels")?.selection;
    }

    const imageryOption: ImageryOption = getCurrentImageryOption(mapSettings);
    if(imageryOption.time != null) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (window.map as any).setConfigProperty('basemap', 'showPlaceLabels', !shouldHide);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (window.map as any).setConfigProperty('basemap', 'showRoadLabels', !shouldHide);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (window.map as any).setConfigProperty('basemap', 'showPointOfInterestLabels', !shouldHide);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (window.map as any).setConfigProperty('basemap', 'showTransitLabels', !shouldHide);
    } else {
        // The above only works when using the "Standard" style from Mapbox v3, if using any
        // other style (such as "Light", or "Dark"), then it will fail. In which case we do it
        // the old fashioned way by toggling individual layers.
        const layers = window.map.getStyle().layers;
		layers.forEach(layer => {
            if(placenameLayers.includes(layer["id"])) {
                window.map.setLayoutProperty(
                    layer["id"],
                    "visibility",
                    (shouldHide ? "none" : "visible")
                );
            }
		});
    }
}

// Default imagery options in case their missing from user's "map-settings.json" file.
const defaultImageryOptions = {
    "default": "Light",
    "options": [
        {
            "name": "Light",
            "url": "mapbox://styles/mapbox/light-v11?optimize=true"
        },
        {
            "name": "Dark",
            "url": "mapbox://styles/mapbox/dark-v11?optimize=true"
        },
        {
            "name": "Outdoors",
            "url": "mapbox://styles/mapbox/outdoors-v12?optimize=true"
        },
        {
            "name": "Satellite",
            "url": "mapbox://styles/mapbox/satellite-streets-v12?optimize=true"
        },
        {
            "name": "3D (Day)",
            "url": "mapbox://styles/mapbox/standard",
            "time": "dawn"
        },
        {
            "name": "3D (Night)",
            "url": "mapbox://styles/mapbox/standard",
            "time": "dusk"
        }
    ]
}

// Mapbox layer names to adjust when toggling placenames
const placenameLayers = [
    "road-number-shield", "road-label", "road-label-simple", "road-intersection", "waterway-label", 
    "natural-point-label", "water-line-label", "water-point-label", "poi-label", "airport-label", 
    "settlement-subdivision-label", "settlement-minor-label", "settlement-major-label", "settlement-label",
    "state-label", "country-label", "road-oneway-arrow-blue", "road-oneway-arrow-white", "transit-label",
    "path-pedestrian-label", "golf-hole-label", "gate-label", "natural-line-label"
]
