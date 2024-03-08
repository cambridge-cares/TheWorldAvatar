// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { setLatLng, setName, setProperties, setSourceLayerId } from 'state/map-feature-slice';

/**
 * Function to add event listeners for the specified Mapbox map.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 */
export function addMapboxEventListeners(map: mapboxgl.Map, dispatch: Dispatch<any>): void {
  // For any movement within the map
  map.on("mousemove", function (e) {
    // Access the first feature under the mouse pointer
    const feature = map.queryRenderedFeatures(e.point)[0];
    const name = feature?.properties.name ?? null;
    const lngLat: mapboxgl.LngLat = e.lngLat;
    // Store the current mouse position coordinates and feature name in a global state
    dispatch(setLatLng({ lat: lngLat.lat, lng: lngLat.lng }));
    dispatch(setName(name));
  });

  // For click events
  map.on("click", (e) => {
    // Accessing the first feature in the array of features under the click point
    const feature = map.queryRenderedFeatures(e.point)[0];
    if (feature) {
      // Stores the feature properties and layer source id in a global state
      dispatch(setProperties(feature.properties));
      dispatch(setSourceLayerId(feature.source));
    }
  });
}