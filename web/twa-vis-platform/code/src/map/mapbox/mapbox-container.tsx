/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */

/**
 * Question(s) here are:
 *
 * - How the hell can I create a map instance that can be interacted with from other classes, or other
 * UI components in a totally different place in the UI hierarchy?
 *     - Quick solution is to use the global "window" object to store the map object globally.
 *
 * - Can I create a map instance that won't re-initialise (and re-load ALL the data) anytime something
 * else in the UI changes?
 */
import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl from 'mapbox-gl';
import React, { useRef, useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getLatLng, getName } from 'state/map-feature-slice';
import { setIsStyleLoaded } from 'state/floating-panel-slice';
import { MapSettings } from 'types/settings';
import { DataStore } from 'io/data/data-store';
import { addAllSources } from './mapbox-source-utils';
import { addAllLayers } from './mapbox-layer-utils';
import { addIcons } from './mapbox-icon-loader';
import { addMapboxEventListeners } from '../event-listeners';
import { getCurrentImageryOption, getDefaultCameraPosition } from '../map-helper';


// Type definition of incoming properties
interface MapProperties {
  settings: MapSettings;
  dataStore: DataStore;
}

/**
 * Dynamically load and render content from an optional metadata file
 * based on the incoming URL route.
 *
 * @param params incoming route parameters.
 *
 * @returns React component for display.
 */
export default function MapboxMapComponent(props: MapProperties) {
  const mapContainer = useRef(null);
  const map = useRef(null);
  const toolTip = useRef(null);
  const dispatch = useDispatch();
  const coordinates = useSelector(getLatLng);
  const name = useSelector(getName);

  // Run when component loaded
  useEffect(() => {
    initialiseMap();
  }, [props.settings]);

  // Run whenever coordinates are changed from moving across the map
  useEffect(() => {
    // Remove the tool tip if it is currently open
    if (toolTip.current?.isOpen()) {
      toolTip.current.remove();
    }
    // If there is a name, add the tool tip with the name to the map
    if (name != null) {
      toolTip.current.setLngLat(coordinates).setHTML(name).addTo(window.map)
    }
  }, [coordinates]);

  // Initialise the map object
  const initialiseMap = async () => {
    if (map.current) return;
    window.type = "mapbox";

    const response = await fetch("/api/map/settings", {
      method: "GET",
      headers: { "Content-Type": "application/json" },
    });
    const respJson = await response.json();
    // Set credentials
    mapboxgl.accessToken = respJson.token;

    // Get default camera position
    const defaultPosition = getDefaultCameraPosition(props.settings.camera);
    let styleObject = getCurrentImageryOption(props.settings.imagery);

    map.current = new mapboxgl.Map({
      container: mapContainer.current,
      style: styleObject.url,
      center: defaultPosition["center"],
      zoom: defaultPosition["zoom"],
      bearing: defaultPosition["bearing"],
      pitch: defaultPosition["pitch"],
    });

    // Store map object globally.
    // Note that setting a globally accessible variable for the map probably isn't wise. However,
    // we know this shouldn't be re-initialised, and the alternative is to pass the map object into
    // dozens of other UI components as a prop. This method should also allow client-side JS scripts
    // to access the map too. Would recommend revisiting this choice later though.
    window.map = map.current;
    console.info("Initialised a new Mapbox map object.");

    // Add all map event listeners
    addMapboxEventListeners(map.current, dispatch, props.dataStore);

    // Create a new pop up and assign it to the reference to keep track
    toolTip.current = new mapboxgl.Popup({
      closeButton: false,
      closeOnClick: false
    });

    // Removes the tool tip once the mouse pointer is outside the map container
    window.map.on("mouseout", function () {
      toolTip.current.remove();
    });

    window.map.on("style.load", function () {
      // Update time if using new v3 standard style
      styleObject = getCurrentImageryOption(props.settings.imagery);
      if (styleObject.time != null) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (window.map as any).setConfigProperty(
          "basemap",
          "lightPreset",
          styleObject.time
        );
      }
      dispatch(setIsStyleLoaded(true))

      // Parse data configuration and load icons
      const iconPromise = addIcons(props.settings.icons);

      Promise.all([iconPromise]).then(() => {
        // Once that is done and completed...
        console.log("Data definitions fetched and parsed.");

        // Plot data
        addAllSources(props.dataStore);
        addAllLayers(props.dataStore, props.settings.imagery);
      });
    });
  };

  return (
    <div id="mapContainer" ref={mapContainer} className="mapContainer">
      {/* Map will be generated here. */}
    </div>
  );
}
