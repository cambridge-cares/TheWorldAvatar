// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { setLatLng, setName, setQueryTrigger, setIri, setProperties, setStack } from 'state/map-feature-slice';
import { DataStore } from 'io/data/data-store';

/**
 * Function to add event listeners for the specified Mapbox map.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 */
export function addMapboxEventListeners(map: mapboxgl.Map, dispatch: Dispatch, dataStore: DataStore) {
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
    // Set up query parameters
    let iri: string = null;
    let stack: string = null;
    let queryTrigger: boolean = false; // Query will not execute by default
    if (!feature?.properties?.iri) {
      console.warn("IRI is missing. Data fetching will be skipped.");
    } else if (!dataStore.getStackEndpoint(feature.source)) {
      console.warn("Feature does not have a defined stack. Data fetching will be skipped.");
    } else {
      iri = feature.properties.iri;
      stack = dataStore.getStackEndpoint(feature.source);
      queryTrigger = true;
    }
    dispatch(setIri(iri));
    dispatch(setProperties(feature.properties));
    dispatch(setStack(stack));
    dispatch(setQueryTrigger(queryTrigger));
  });
}