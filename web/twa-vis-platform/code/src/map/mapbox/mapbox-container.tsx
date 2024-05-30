/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */
import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl, { Map } from 'mapbox-gl';
import React, { useRef, useEffect } from 'react';

import { MapSettings } from 'types/settings';
import { getCurrentImageryOption, getDefaultCameraPosition } from '../map-helper';
import { formatAppUrl } from 'utils/client-utils';
import MapEventManager from 'map/event-listeners';

// Type definition of incoming properties
interface MapProperties {
  settings: MapSettings;
  currentMap: Map;
  setMap: React.Dispatch<React.SetStateAction<Map>>;
  setMapEventManager: React.Dispatch<React.SetStateAction<MapEventManager>>;
}

/**
 * Renders a mapbox map instance. 
 *
 * @param params incoming route parameters.
 *
 * @returns React component for display.
 */
export default function MapboxMapComponent(props: MapProperties) {
  const mapContainer = useRef(null);

  // Run when component loaded
  useEffect(() => {
    initialiseMap();

    return () => {
      if (props.currentMap) {
        props.currentMap.remove(); // Remove the map instance
        props.setMap(null); // Reset the map ref
      }
    };
  }, []);

  // Initialise the map object
  const initialiseMap = async () => {
    props.currentMap?.remove();

    const response = await fetch(formatAppUrl("/api/map/settings"), {
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

    map.addControl(new mapboxgl.ScaleControl(), "bottom-right");
    map.addControl(new mapboxgl.NavigationControl(), "bottom-right");

    console.info("Initialised a new Mapbox map object.");

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
      // Map is only settable after the styles have loaded
      props.setMap(map);
      props.setMapEventManager(new MapEventManager(map));
    });
  };

  return (
    <div id="mapContainer" ref={mapContainer} className="mapContainer">
      {/* Map will be generated here. */}
    </div>
  );
}

MapboxMapComponent.displayName = "MapboxMapComponent";