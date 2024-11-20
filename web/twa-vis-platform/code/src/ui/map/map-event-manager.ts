// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl, { MapEvent, MapMouseEvent } from 'mapbox-gl';

import { Interactions } from 'io/config/interactions';
import { DataStore } from 'io/data/data-store';
import { Dispatch } from 'react';
import { setIndex } from 'state/floating-panel-slice';
import { addFeatures, clearFeatures, MapFeaturePayload } from 'state/map-feature-slice';

type ActionType = ReturnType<typeof clearFeatures> | ReturnType<typeof addFeatures> | ReturnType<typeof setIndex>;

interface TWAFeature {
  id: string;
  properties: {
    [key: string]: string;
  };
}

/**
 * This class is a singleton intended to manage custom event listeners in a separate state to simplify the removal of these listeners.
 */
export default class MapEventManager {
  private map: mapboxgl.Map = null;
  private listeners: Array<{ event: MapEvent; listener: (_e: MapMouseEvent) => void; layerID?: string }> = [];

  // Constructs an instance of this event manager
  public constructor(map: mapboxgl.Map) {
    this.map = map;
  }

  /**
   * Function to add event listeners for the specified Mapbox map.
   * 
   * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
   * @param {DataStore} dataStore - The data store.
   */
  public addMapboxEventListeners(dispatch: Dispatch<ActionType>, dataStore: DataStore) {
    this.removeAllEventListeners();
    this.addFeatureClickEventListener(dispatch, dataStore);
    this.addTooltipEventListener();
    this.addHoverEventListener(dataStore);
  }

  /**
   * Function to add an event listener for clicking on a feature.
   * 
   * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
   * @param {DataStore} dataStore - The data store storing the map layer information.
   */

  private addFeatureClickEventListener(dispatch: Dispatch<ActionType>, dataStore: DataStore) {
    this.addEventListener({ type: "click", target: this.map }, (event) => {
      const e = event as MapMouseEvent;

      // Reset features upon clicked
      dispatch(clearFeatures());
      // Store all clickable features within the clicked radius with some additional metadata
      const features: MapFeaturePayload[] = this.map.queryRenderedFeatures(e.point)
        .filter((feature) =>
          dataStore && feature.layer && dataStore.getLayerWithID(feature.layer.id)?.getInjectableProperty(Interactions.CLICKABLE)?.style[0]
        )
        .map((feature) => {
          const twaFeature = feature as unknown as TWAFeature; // Correct casting
          return {
            ...twaFeature.properties,
            name: twaFeature.properties.name ?? (twaFeature.id !== undefined ? "Feature #" + twaFeature.id : "Feature"),
            stack: dataStore?.getStackEndpoint(feature.source), // Store the associated stack if available
            layer: dataStore?.getLayerWithID(feature.layer.id)?.name, // Store the layer's public-facing name
          };
        });

      dispatch(addFeatures(features));

      // Switch to the info tab at index 2 only if the click event occurs with at least one feature
      if (features.length > 0) {
        dispatch(setIndex({ index: 2 }));
      }
    });
  }

  /**
   * Function to add event listeners to show a tooltip when hovering over a feature.
   */
  private addTooltipEventListener() {
    const map: mapboxgl.Map = this.map;
    // Create a new pop up and assign it to the reference to keep track
    const toolTip: mapboxgl.Popup = new mapboxgl.Popup({
      closeButton: false,
      closeOnClick: false
    });
    // For any movement within the map, a tool tip should appear when hovering a feature
    this.addEventListener({ type: "mousemove", target: this.map }, (event) => {
      const e = event as MapMouseEvent;


      // Remove the tool tip if it is currently open
      if (toolTip?.isOpen()) {
        toolTip.remove();
      }
      // Access the first feature under the mouse pointer
      const feature = map.queryRenderedFeatures(e.point)[0];
      const twaFeature = feature as unknown as TWAFeature
      const name = twaFeature?.properties.name ?? null;
      let description = twaFeature?.properties.description ?? null;
      // Handle line breaks in description text
      description = description?.replace(/\n/g, '<br>');
      const lngLat: mapboxgl.LngLat = e.lngLat;
      // If there is a name, add the tool tip with the name to the map
      if (name) {
        let html: string = `<h1>${name}</h1>`;
        if (description) { html += `<i>${description}</i>` }
        toolTip?.setLngLat(lngLat).setHTML(html).addTo(map);
      }

    });

    // Removes the tool tip once the mouse pointer is outside the map container
    this.addEventListener({ type: "mouseout", target: this.map }, () => {
      toolTip.remove();
    });
  }

  /**
   * Function to add event listeners for layers with hovering enabled.
   * 
   * @param {DataStore} dataStore - The data store storing the map layer information.
   */
  private addHoverEventListener(dataStore: DataStore) {
    const map: mapboxgl.Map = this.map;
    dataStore?.getLayerList().map(layer => {
      if (layer.hasInjectableProperty(Interactions.HOVER)) {
        const hoverProperty = layer.getInjectableProperty(Interactions.HOVER).style ;
        // Updates the conditional paint property with the IRI of the currently hovering feature
        this.addEventListener({ type: "mousemove", target: this.map }, (event) => {
          const e = event as MapMouseEvent;
          const feature = map.queryRenderedFeatures(e.point)[0];
          const twaFeature = feature as unknown as TWAFeature;
          const prevIri: string = hoverProperty[1][2] as string; 
          if (twaFeature.properties?.iri != prevIri) {
            hoverProperty[1][2] = twaFeature.properties?.iri as string;
          }
          map.setPaintProperty(layer.id, "fill-opacity", hoverProperty as unknown as number);
        }, layer.id);

        // When hovering outside the layer, reset the property to ensure highlight is removed
        this.addEventListener({ type: "mouseleave", target: this.map }, function () {
          hoverProperty[1][2] = "[HOVERED-IRI]" as string;
          map.setPaintProperty(layer.id, "fill-opacity", hoverProperty as unknown as number);
        }, layer.id);
      }
    });
  }

  /**
   * Main function to add any event listener to the current map. The manager will cache these listeners for ease of removal.
   * 
   * @param {MapMouseEvent} event - Event of interest.
   * @param {(e: MapMouseEvent) => void} listener - Event listener to be added.
   * @param {string} layerID - Optional parameter for the specified layer id if available.
   */
  private addEventListener(event: MapEvent, listener: (_e: MapEvent) => void, layerID?: string): void {
    if (layerID) {
      // Assuming `this.map.on` expects a listener of a specific type, ensure `listener` matches.
      // This might require wrapping `listener` to match the expected signature.
      this.map.on(event.type, layerID, listener);
    } else {
      this.map.on(event.type, listener);
    }
    this.listeners.push({ event: event, listener, layerID });
  }

  /**
   * Function to remove all data-specific event listeners added to the current map.
   */
  private removeAllEventListeners(): void {
    this.listeners.forEach(({ event, layerID, listener }) => {
      const l = listener as (_e: MapEvent) => void
      if (layerID) {
        this.map.off(event.type, layerID, l);
      } else {
        this.map.off(event.type, l);
      }
    });
    this.listeners = [];
  }
}