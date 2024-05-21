/**
 * Map-related utility methods including camera and imagery operations.
 */

"use client";

import 'react-toastify/dist/ReactToastify.css';
import { toast } from 'react-toastify';
import { Map } from 'mapbox-gl';

import { reduxStore } from 'app/store';
import { DataStore } from 'io/data/data-store';
import { CameraSettings, CameraPosition, ImagerySettings, ImageryOption, MapSettings } from 'types/settings';
import { addIcons } from './mapbox/mapbox-icon-loader';
import { addAllSources } from './mapbox/mapbox-source-utils';
import { addAllLayers } from './mapbox/mapbox-layer-utils';

// Default imagery options if users do not include them in the "map-settings.json" file
const DEFAULT_IMAGERY_OPTIONS: ImagerySettings = {
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

// Layer names adopted by Mapbox that will be hidden or shown when toggling place names in our visualisation
const PLACENAME_LAYERS: string[] = [
  "road-number-shield", "road-label", "road-label-simple", "road-intersection", "waterway-label",
  "natural-point-label", "water-line-label", "water-point-label", "poi-label", "airport-label",
  "settlement-subdivision-label", "settlement-minor-label", "settlement-major-label", "settlement-label",
  "state-label", "country-label", "road-oneway-arrow-blue", "road-oneway-arrow-white", "transit-label",
  "path-pedestrian-label", "golf-hole-label", "gate-label", "natural-line-label"
]

/**
 * Add data to the map.
 * 
 * @param {Map} map The current Mapbox map instance.
 * @param {MapSettings} mapSettings The user specified map settings.
 * @param {DataStore} data The data of interest to add to the map.
 */
export function addData(map: Map, mapSettings: MapSettings, data: DataStore): void {
   // Parse data configuration and load icons
   const iconPromise = addIcons(map, mapSettings.icons);

   Promise.all([iconPromise]).then(() => {
     // Once that is done and completed...
     console.log("Data definitions fetched and parsed.");

     // Plot data
     addAllSources(map, data);
     addAllLayers(map, data, mapSettings.imagery);
   });
}

/**
 * Returns the default camera position object.
 * 
 * @param cameraSettings Camera settings object.
 * 
 * @returns Default CameraPosition object.
 */
export function getDefaultCameraPosition(cameraSettings: CameraSettings): CameraPosition {
  const defaultName = cameraSettings.default;
  return getCameraPosition(defaultName, cameraSettings);
}

/**
 * Returns the camera position object that matches the input name.
 * 
 * @param name Camera position name.
 * @param cameraSettings Camera settings object.
 * @returns Corresponding CameraPosition (or null).
 */
export function getCameraPosition(name: string, cameraSettings: CameraSettings): CameraPosition {
  const positions = cameraSettings.positions;
  return positions.find(item => item["name"] === name);
}

/**
 * Returns available names of all camera positions.
 * 
 * @param cameraSettings Camera settings object.
 * @returns A list of the names for all camera positions.
 */
export function getCameraPositions(cameraSettings: CameraSettings): string[] {
  return cameraSettings.positions.map((position: CameraPosition) => position.name);
}

/**
 * Returns the default imagery option.
 * 
 * @param imagerySettings Imagery settings object.
 * 
 * @returns Default ImageryOption object.
 */
export function getDefaultImageryOption(imagerySettings: ImagerySettings): ImageryOption {
  // If users do not specify imagery settings, use the defaults
  if (imagerySettings == null) {
    imagerySettings = DEFAULT_IMAGERY_OPTIONS;
  }

  if (typeof window !== "undefined" && imagerySettings.default.toLowerCase() == "auto") {
    // Auto detect browser theme
    if (window?.matchMedia && window?.matchMedia('(prefers-color-scheme: dark)').matches) {
      return getImageryOption("3D (Night)", imagerySettings);
    } else {
      return getImageryOption("3D (Day)", imagerySettings);
    }
  } else {
    return getImageryOption(imagerySettings.default, imagerySettings);
  }
}

/**
 * Returns the imagery option that matches the input name.
 * 
 * @param name Imagery option name.
 * @param imagerySettings Imagery settings object.
 * @returns Corresponding ImageryOption (or null).
 */
function getImageryOption(name: string, imagerySettings: ImagerySettings): ImageryOption {
  if (imagerySettings == null) {
    imagerySettings = DEFAULT_IMAGERY_OPTIONS;
  }

  const options = imagerySettings.options;
  return options.find(item => item["name"] === name);
}

/**
 * Returns names of all available imagery options.
 * 
 * @param imagerySettings Imagery settings object.
 * @returns A list of the names for all available imagery options.
 */
export function getImageryOptions(imagery: ImagerySettings): string[] {
  return imagery.options.map((option: ImageryOption) => option.name);
}

/**
 * Returns the currently selected ImageryOption object.
 * 
 * @param imagerySettings Imagery settings object.
 * @returns ImageryOption for current selection.
 */
export function getCurrentImageryOption(imagerySettings: ImagerySettings): ImageryOption {
  const reduxState = reduxStore.getState();
  const items = reduxState.ribbonComponents.items;
  if (items == null || items.length == 0) {
    return getDefaultImageryOption(imagerySettings);
  } else {
    const match = items.find(option => option.name === "Imagery");
    if (match == null) {
      return getDefaultImageryOption(imagerySettings);
    } else {
      return getImageryOption(match.selection, imagerySettings);
    }
  }
}

/**
 * Set the base map imagery to the current option.
 * 
 * @param {Map} map The current Mapbox map instance.
 */
export function setImagery(imagerySettings: ImagerySettings, map: Map): void {
  const imageryOption: ImageryOption = getCurrentImageryOption(imagerySettings);

  // Update map
  map.setStyle(imageryOption.url);
  map.setProjection({
    name: 'mercator'
  });

  map.on('style.load', () => {
    if (imageryOption.time != null) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (map as any).setConfigProperty('basemap', 'lightPreset', imageryOption.time);
    }

    // Ensure placenames match previous state
    togglePlacenames(imagerySettings, map);
  });
}

/**
 * Toggle the display of all placenames on the map based on the current imagery option.
 * 
 * @param {ImagerySettings} imageryOption Current imagery option object.
 * @param {Map} map The current Mapbox map instance.
 */
export function togglePlacenames(imagerySettings: ImagerySettings, map: Map): void {
  const reduxState = reduxStore.getState();
  const items = reduxState.ribbonComponents.items;
  let shouldHide = true;
  if (items != null && items.length > 0) {
    shouldHide = items.find(option => option.name === "Hide Labels")?.selection;
  }
  const imageryOption: ImageryOption = getCurrentImageryOption(imagerySettings);

  if (imageryOption.time != null) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (map as any).setConfigProperty('basemap', 'showPlaceLabels', !shouldHide);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (map as any).setConfigProperty('basemap', 'showRoadLabels', !shouldHide);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (map as any).setConfigProperty('basemap', 'showPointOfInterestLabels', !shouldHide);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (map as any).setConfigProperty('basemap', 'showTransitLabels', !shouldHide);
  } else {
    // The above only works when using the "Standard" style from Mapbox v3, if using any
    // other style (such as "Light", or "Dark"), then it will fail. In which case we do it
    // the old fashioned way by toggling individual layers.
    const layers = map.getStyle().layers;
    layers.forEach(layer => {
      if (PLACENAME_LAYERS.includes(layer["id"])) {
        map.setLayoutProperty(
          layer["id"],
          "visibility",
          (shouldHide ? "none" : "visible")
        );
      }
    });
  }
}

/**
 * Toggle's 3D terrain.
 * 
 * @param {boolean} state toggle flag
 * @param {Map} map The current Mapbox map instance.
 */
export function set3DTerrain(state: boolean, map: Map): void {
  if (state) {
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
 * 
 * @param {CameraSettings} cameraSettings The camera settings.
 * @param {Map} map The current Mapbox map instance.
 */
export function resetCamera(cameraSettings: CameraSettings, map: Map): void {
  const reduxState = reduxStore.getState();
  const items = reduxState.ribbonComponents.items;
  let position: CameraPosition;
  if (items.length == 0) {
    position = getDefaultCameraPosition(cameraSettings);
  } else {
    const positionName = items.find(position => position.name === "Reset Camera")?.selection;
    position = getCameraPosition(positionName, cameraSettings);
  }

  // Move the map
  map.flyTo({
    ...position,
    essential: true
  });
}

/**
 * If given permission by browser alert, this moves the map to the user's location.
 * 
 * @param {Map} map The current Mapbox map instance.
 */
export function locateUser(map: Map): void {
  navigator.geolocation.getCurrentPosition(
    (geolocation) => {
      const long = geolocation["coords"]["longitude"];
      const lat = geolocation["coords"]["latitude"];

      // Move the map
      map.flyTo({
        center: [long, lat],
        zoom: 12,
        essential: true
      });
    },
    () => {
      toast.warning(
        "Cannot read user's location without browser authorisation.",
        { position: toast.POSITION.BOTTOM_LEFT }
      )
    }
  );
}