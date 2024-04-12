/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */
import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl, { Map } from 'mapbox-gl';
import React, { useRef, useEffect, forwardRef } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { getLatLng, getName } from 'state/map-feature-slice';
import { setIsStyleLoaded } from 'state/floating-panel-slice';
import { MapSettings } from 'types/settings';
import { getCurrentImageryOption, getDefaultCameraPosition } from '../map-helper';

// Type definition of incoming properties
interface MapProperties {
  settings: MapSettings;
}

/**
 * Renders a mapbox map instance. 
 *
 * @param params incoming route parameters.
 *
 * @returns React component for display.
 */
export const MapboxMapComponent = forwardRef<Map, MapProperties>((props, mapRef) => {
  const mapContainer = useRef(null);
  const toolTip = useRef(null);
  const dispatch = useDispatch();
  const coordinates = useSelector(getLatLng);
  const name = useSelector(getName);

  // Run when component loaded
  useEffect(() => {
    initialiseMap();

    return () => {
      const currentMap: React.MutableRefObject<Map> = mapRef as React.MutableRefObject<Map>;
      if (currentMap.current) {
        currentMap.current.remove(); // Remove the map instance
        currentMap.current = null; // Reset the map ref
      }
    };
  }, []);

  // Run whenever coordinates are changed from moving across the map
  useEffect(() => {
    // Remove the tool tip if it is currently open
    if (toolTip.current?.isOpen()) {
      toolTip.current.remove();
    }
    // If there is a name, add the tool tip with the name to the map
    if (name != null) {
      toolTip.current?.setLngLat(coordinates).setHTML(name).addTo((mapRef as React.MutableRefObject<Map>).current)
    }
  }, [coordinates]);

  // Initialise the map object
  const initialiseMap = async () => {
    if ((mapRef as React.MutableRefObject<Map>).current) return;

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

    const map: Map = new mapboxgl.Map({
      container: mapContainer.current,
      style: styleObject.url,
      center: defaultPosition["center"],
      zoom: defaultPosition["zoom"],
      bearing: defaultPosition["bearing"],
      pitch: defaultPosition["pitch"],
    });
    (mapRef as React.MutableRefObject<Map>).current = map;
    console.info("Initialised a new Mapbox map object.");

    // Create a new pop up and assign it to the reference to keep track
    toolTip.current = new mapboxgl.Popup({
      closeButton: false,
      closeOnClick: false
    });

    // Removes the tool tip once the mouse pointer is outside the map container
    map.on("mouseout", function () {
      toolTip.current.remove();
    });

    map.on("style.load", function () {
      // Update time if using new v3 standard style
      styleObject = getCurrentImageryOption(props.settings.imagery);
      if (styleObject.time != null) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (map as any).setConfigProperty(
          "basemap",
          "lightPreset",
          styleObject.time
        );
      }
      dispatch(setIsStyleLoaded(true))
    });
  };

  return (
    <div id="mapContainer" ref={mapContainer} className="mapContainer">
      {/* Map will be generated here. */}
    </div>
  );
});

MapboxMapComponent.displayName = "MapboxMapComponent";