// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { setLatLng, setProperties, setSourceLayerId } from 'state/map-feature-slice';

/**
 * Function to add event listeners for the specified Mapbox map.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 */
export function addMapboxEventListeners(map: mapboxgl.Map, dispatch: Dispatch<any>): void {
  // For click events
  map.on("click", function (e) {
    // Stores the latitude and longitude of the clicked location in a global state
    dispatch(setLatLng({ lat: e.lngLat.lat, lng: e.lngLat.lng }));
  });
}

/**
 * Function to add all event listeners for the specified Mapbox map layer.

 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {string} layerId - The ID of the Mapbox layer to listen for click events on.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 */
export function addMapboxLayerEventListeners(map: mapboxgl.Map, layerId: string, dispatch: Dispatch<any>): void {
  // For click events
  map.on('click', layerId, (e) => {
    // Accessing the first feature in the array of features under the click point
    const feature = e.features && e.features[0];

    if (feature) {
      // Here you can access the metadata of the clicked feature
      console.log(`Clicked on ${layerId}:`, feature.properties);
      // Stores the feature properties and layer source id in a global state
      dispatch(setProperties(feature.properties));
      dispatch(setSourceLayerId(feature.source));
    }
  });
}