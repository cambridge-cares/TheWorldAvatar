/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */
import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl, { Map } from 'mapbox-gl';
import React, { useEffect, useRef } from 'react';

import { Apis } from 'io/config/routes';
import { CameraPosition, ImageryOption } from 'types/settings';

// Type definition of incoming properties
interface MapProperties {
  currentMap: Map;
  styles: string;
  setMap: React.Dispatch<React.SetStateAction<Map>>;
  defaultPosition: CameraPosition;
  imageryOption?: ImageryOption;
}

/**
 * Renders a mapbox map instance. 
 *
 * @param {Map} map The reference to the current map (if any).
 * @param {string} styles The css styles for the mapbox container.
 * @param setMap Sets the reference for the created map.
 * @param {CameraPosition} defaultPosition The default camera position for the map.
 * @param {ImageryOption} imageryOption An optional imagery option for the default map setup.
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

    const defaultImagery: ImageryOption = props.imageryOption ?? {
      "name": "Light",
      "url": "mapbox://styles/mapbox/light-v11?optimize=true"
    };

    const response = await fetch((Apis.MAP_SETTINGS), {
      method: "GET",
      headers: { "Content-Type": "application/json" },
    });
    const respJson = await response.json();
    // Set credentials
    mapboxgl.accessToken = respJson.token;

    const map: Map = new mapboxgl.Map({
      container: mapContainer.current,
      style: defaultImagery.url,
      center: props.defaultPosition.center,
      zoom: props.defaultPosition.zoom,
      bearing: props.defaultPosition.bearing,
      pitch: props.defaultPosition.pitch,
    });

    map.addControl(new mapboxgl.ScaleControl() as mapboxgl.IControl, "bottom-right");
    map.addControl(new mapboxgl.NavigationControl(), "bottom-right");

    console.info("Initialised a new Mapbox map object.");

    map.on("style.load", function () {
      // Update time if using new v3 standard style
      if (defaultImagery.time != null) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (map as any).setConfigProperty(
          "basemap",
          "lightPreset",
          defaultImagery.time
        );
      }
      // Map is only settable after the styles have loaded
      props.setMap(map);
    });
  };

  return (
    <div id="mapContainer" ref={mapContainer} className={props.styles}>
      {/* Map will be generated here. */}
    </div>
  );
}

MapboxMapComponent.displayName = "MapboxMapComponent";