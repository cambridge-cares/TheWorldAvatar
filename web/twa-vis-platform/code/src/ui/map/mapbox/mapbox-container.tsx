/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable react/prop-types */
import 'mapbox-gl/dist/mapbox-gl.css';
import './mapbox.css';

import mapboxgl, { Map } from 'mapbox-gl';
import React, { useEffect, useRef } from 'react';

import { CameraPosition, ImageryOption } from 'types/settings';
import MapEventManager from 'ui/map/map-event-manager';

// Type definition of incoming properties
interface MapProperties {
  currentMap: Map;
  defaultPosition: CameraPosition;
  imageryOption?: ImageryOption;
  setMap: React.Dispatch<React.SetStateAction<Map>>;
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

    const defaultImagery: ImageryOption = props.imageryOption ?? {
      "name": "Light",
      "url": "mapbox://styles/mapbox/light-v11?optimize=true"
    };

    const response = await fetch(("./api/map/settings"), {
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
    <div id="mapContainer" ref={mapContainer} className="mapContainer">
      {/* Map will be generated here. */}
    </div>
  );
}

MapboxMapComponent.displayName = "MapboxMapComponent";