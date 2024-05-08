// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { setQueryTrigger, setIri, setProperties, setStack } from 'state/map-feature-slice';
import { DataStore } from 'io/data/data-store';

/**
 * Function to add event listeners for the specified Mapbox map.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 * @param {DataStore} dataStore - The data store.
 */
export function addMapboxEventListeners(map: mapboxgl.Map, dispatch: Dispatch, dataStore: DataStore) {
  addTooltipEventListener(map);

  // For click events
  map.on("click", (e) => {
    // Accessing the first feature in the array of features under the click point
    const feature = map.queryRenderedFeatures(e.point)[0];
    // Set up query parameters
    let iri: string = null;
    let stack: string = null;
    if (!feature?.properties?.iri) {
      console.warn("IRI is missing. Data fetching will be skipped.");
    } else if (!dataStore.getStackEndpoint(feature.source)) {
      console.warn("Feature does not have a defined stack. Data fetching will be skipped.");
    } else {
      iri = feature.properties.iri;
      stack = dataStore.getStackEndpoint(feature.source);
    }
    dispatch(setIri(iri));
    dispatch(setProperties(feature?.properties));
    dispatch(setStack(stack));
    dispatch(setQueryTrigger(true));
  });
}

/**
 * Function to add event listeners to show a tooltip when hovering over a feature.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 */
function addTooltipEventListener(map: mapboxgl.Map) {
  // Create a new pop up and assign it to the reference to keep track
  const toolTip: mapboxgl.Popup = new mapboxgl.Popup({
    closeButton: false,
    closeOnClick: false
  });

  // For any movement within the map, a tool tip should appear when hovering a feature
  map.on("mousemove", function (e) {
    // Remove the tool tip if it is currently open
    if (toolTip?.isOpen()) {
      toolTip.remove();
    }
    // Access the first feature under the mouse pointer
    const feature = map.queryRenderedFeatures(e.point)[0];
    const name = feature?.properties.name ?? null;
    const lngLat: mapboxgl.LngLat = e.lngLat;
    // If there is a name, add the tool tip with the name to the map
    if (name != null) {
      toolTip?.setLngLat(lngLat).setHTML(name).addTo(map);
    }
  });

  // Removes the tool tip once the mouse pointer is outside the map container
  map.on("mouseout", function () {
    toolTip.remove();
  });
}