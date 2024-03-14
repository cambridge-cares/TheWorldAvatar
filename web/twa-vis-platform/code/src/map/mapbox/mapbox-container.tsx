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
"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl from 'mapbox-gl';
import React, { useRef, useEffect } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getLatLng, getName } from 'state/map-feature-slice';
import { MapSettings } from 'types/map-settings';
import { DataStore } from 'io/data/data-store';
import { getDefaultCameraPosition } from './mapbox-camera-utils';
import { getCurrentImageryOption, getDefaultImageryOption, getImageryOption } from './mapbox-imagery-utils';
import { addAllSources } from './mapbox-source-utils';
import { addAllLayers } from './mapbox-layer-utils';
import { addIcons } from './mapbox-icon-loader';
import { addMapboxEventListeners } from '../event-listeners';
import { setIsStyleLoaded } from 'state/floating-panel-slice';


// Type definition of incoming properties
interface MapProperties {
  settings: MapSettings;
  dataStore: DataStore;
}

// Return the default style URL
function getDefaultStyle(mapSettings: MapSettings) {
  if (mapSettings.imagery.default.toLowerCase() == "auto") {
    // Auto detect browser theme
    if (
      window?.matchMedia &&
      window?.matchMedia("(prefers-color-scheme: dark)").matches
    ) {
      return getImageryOption("3D (Night)", mapSettings.imagery);
    } else {
      return getImageryOption("3D (Day)", mapSettings.imagery);
    }
  } else {
    return getDefaultImageryOption(mapSettings);
  }
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
  const settings = props.settings;
  const mapContainer = useRef(null);
  const map = useRef(null);
  const toolTip = useRef(null);
  const dispatch = useDispatch();
  const coordinates = useSelector(getLatLng);
  const name = useSelector(getName);

  // Run when component loaded
  useEffect(() => {
    initialiseMap();
  }, []);

  // Run whenever coordinates are changed from moving across the map
  useEffect(() => {
    // Remove the tool tip if it is currently open
    if (toolTip.current.isOpen()) {
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

    // Set credentials
    mapboxgl.accessToken = settings["credentials"]["key"];

    // Get default camera position
    const defaultPosition = getDefaultCameraPosition(settings);
    let styleObject = getCurrentImageryOption(settings);

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
      styleObject = getCurrentImageryOption(settings);
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
      const iconPromise = addIcons(settings.icons);

      Promise.all([iconPromise]).then(() => {
        let dataStore: DataStore = props.dataStore;

        // Once that is done and completed...
        console.log("Data definitions fetched and parsed.");

        // Plot data
        addAllSources(dataStore);
        addAllLayers(dataStore, dispatch);
      });
    });
  };

  return (
    <div id="mapContainer" ref={mapContainer} className="mapContainer">
      {/* Map will be generated here. */}
    </div>
  );
}
