// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { addFeatures, clearFeatures, MapFeaturePayload } from 'state/map-feature-slice';
import { DataStore } from 'io/data/data-store';
import { Interactions } from 'io/config/interactions';

/**
 * Function to add event listeners for the specified Mapbox map.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 * @param {DataStore} dataStore - The data store.
 */
export function addMapboxEventListeners(map: mapboxgl.Map, dispatch: Dispatch, dataStore: DataStore) {
  addTooltipEventListener(map);
  addHoverEventListener(map, dataStore);
  // For click events
  map.on("click", (e) => {
    // Reset features upon clicked
    dispatch(clearFeatures());
    // Store all features within the clicked radius with some additional metadata
    const features: MapFeaturePayload[] = map.queryRenderedFeatures(e.point).map((feature) => ({
      ...feature.properties,
      name: feature.properties.name ?? (feature.id !== undefined ? "Feature #" + feature.id : "Feature"),
      stack: dataStore?.getStackEndpoint(feature.source), // Store the associated stack if available
      layer: dataStore?.getLayerWithID(feature.layer.id).name, // Store the layer's public-facing name
    }))
    dispatch(addFeatures(features));
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

/**
 * Function to add event listeners for layers with hovering enabled.
 * 
 * @param {mapboxgl.Map} map - The Mapbox map object to attach the event listener to.
 * @param {DataStore} dataStore - The data store storing the map layer information.

 */
function addHoverEventListener(map: mapboxgl.Map, dataStore: DataStore) {
  dataStore?.getLayerList().map(layer => {
    if (layer.hasInjectableProperty(Interactions.HOVER)) {
      const hoverProperty = layer.getInjectableProperty(Interactions.HOVER).style;
      // Updates the conditional paint property with the IRI of the currently hovering feature
      map.on("mousemove", layer.id, function (e) {
        const feature = map.queryRenderedFeatures(e.point)[0];
        const prevIri: string = hoverProperty[1][2];
        if (feature.properties?.iri != prevIri) {
          hoverProperty[1][2] = feature.properties?.iri;
        }
        map.setPaintProperty(layer.id, "fill-opacity", hoverProperty);
      });

      // When hovering outside the layer, reset the property to ensure highlight is removed
      map.on("mouseleave", layer.id, function () {
        hoverProperty[1][2] = "[HOVERED-IRI]";
        map.setPaintProperty(layer.id, "fill-opacity", hoverProperty);
      });
    }
  })
}