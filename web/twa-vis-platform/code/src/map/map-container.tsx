"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './map-container.module.css';

import React from 'react';

import { MapSettings } from 'types/settings';
import { DataStore } from 'io/data/data-store';
import { parseMapDataSettings } from 'utils/client-utils';
import MapboxMapComponent from 'map/mapbox/mapbox-container';
import Ribbon from 'app/visualisation/ribbon/ribbon';
import FloatingPanelContainer from 'ui/tree/floating-panel';

// Type definition of incoming properties
interface MapContainerProps {
  settings: string;
}

/**
 * Renders the map and its UI components
 */
export default function MapContainer(props: MapContainerProps) {
  const mapSettings: MapSettings = JSON.parse(props.settings);
  const dataStore: DataStore = parseMapDataSettings(mapSettings.data, mapSettings?.type);

  return (
    <>
      {/* Mapbox map */}
      {mapSettings?.["type"] === "mapbox" &&
        <MapboxMapComponent
          settings={mapSettings}
          dataStore={dataStore}
        />
      }

      {/* Cesium map */}
      {mapSettings?.["type"] === "cesium" &&
        <div></div>
      }

      {/* Container elements */}
      <div className={styles.componentContainer}>
        {/* Map controls ribbon */}
        <Ribbon startingIndex={0} mapSettings={mapSettings} />

        {/* Containers for upcoming components (layer tree, metadata, time series charts etc.) */}
        <div className={styles.upperContainer}>
          <FloatingPanelContainer dataStore={dataStore} />
        </div>
        <div className={styles.lowerContainer} />
      </div>
    </>
  )
}