// Import necessary Mapbox GL JS types for TypeScript
import mapboxgl from 'mapbox-gl';
import { Dispatch } from 'redux';

import { addFeatures, clearFeatures, MapFeaturePayload } from 'state/map-feature-slice';
import { DataStore } from 'io/data/data-store';
import { Interactions } from 'io/config/interactions';

/**
 * This class is a singleton intended to manage custom event listeners in a separate state to simplify the removal of these listeners.
 */
export default class MapEventManager {
  private map: mapboxgl.Map = null;
  private listeners: Array<{ event: keyof mapboxgl.MapLayerEventType; layerID?: string; listener: (e: mapboxgl.MapLayerEventType[keyof mapboxgl.MapLayerEventType]) => void }> = [];

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
  public addMapboxEventListeners(dispatch: Dispatch, dataStore: DataStore) {
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
  private addFeatureClickEventListener(dispatch: Dispatch, dataStore: DataStore) {
    this.addEventListener("click", (e) => {
      // Reset features upon clicked
      dispatch(clearFeatures());
      // Store all clickable features within the clicked radius with some additional metadata
      const features: MapFeaturePayload[] = this.map.queryRenderedFeatures(e.point).filter((feature) =>
        dataStore?.getLayerWithID(feature.layer.id).getInjectableProperty(Interactions.CLICKABLE).style[0]
      ).map((feature) => ({
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
   */
  private addTooltipEventListener() {
    const map: mapboxgl.Map = this.map;
    // Create a new pop up and assign it to the reference to keep track
    const toolTip: mapboxgl.Popup = new mapboxgl.Popup({
      closeButton: false,
      closeOnClick: false
    });
    // For any movement within the map, a tool tip should appear when hovering a feature
    this.addEventListener("mousemove", function (e) {
      // Remove the tool tip if it is currently open
      if (toolTip?.isOpen()) {
        toolTip.remove();
      }
      // Access the first feature under the mouse pointer
      const feature = map.queryRenderedFeatures(e.point)[0];
      const name = feature?.properties.name ?? null;
      let description = feature?.properties.description ?? null;
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
    this.addEventListener("mouseout", function () {
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
        const hoverProperty = layer.getInjectableProperty(Interactions.HOVER).style;
        // Updates the conditional paint property with the IRI of the currently hovering feature
        this.addEventListener("mousemove", function (e) {
          const feature = map.queryRenderedFeatures(e.point)[0];
          const prevIri: string = hoverProperty[1][2];
          if (feature.properties?.iri != prevIri) {
            hoverProperty[1][2] = feature.properties?.iri;
          }
          map.setPaintProperty(layer.id, "fill-opacity", hoverProperty);
        }, layer.id);

        // When hovering outside the layer, reset the property to ensure highlight is removed
        this.addEventListener("mouseleave", function () {
          hoverProperty[1][2] = "[HOVERED-IRI]";
          map.setPaintProperty(layer.id, "fill-opacity", hoverProperty);
        }, layer.id);
      }
    });
  }

  /**
   * Main function to add any event listener to the current map. The manager will cache these listeners for ease of removal.
   * 
   * @param {keyof mapboxgl.MapLayerEventType} event - Event of interest.
   * @param {(e: mapboxgl.MapLayerEventType[keyof mapboxgl.MapLayerEventType]) => void} listener - Event listener to be added.
   * @param {string} layerID - Optional parameter for the specified layer id if available.
   */
  private addEventListener(event: keyof mapboxgl.MapLayerEventType, listener: (e: mapboxgl.MapLayerEventType[keyof mapboxgl.MapLayerEventType]) => void, layerID?: string): void {
    if (layerID) {
      this.map.on(event, layerID, listener);
    } else {
      this.map.on(event, listener);
    }
    this.listeners.push({ event, layerID, listener });
  }

  /**
   * Function to remove all data-specific event listeners added to the current map.
   */
  private removeAllEventListeners(): void {
    this.listeners.forEach(({ event, layerID, listener }) => {
      if (layerID) {
        this.map.off(event, layerID, listener);
      } else {
        this.map.off(event, listener);
      }
    });
    this.listeners = [];
  }
}