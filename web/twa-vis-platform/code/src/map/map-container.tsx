"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './map-container.module.css';

import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';

import { MapSettings } from 'types/settings';
import { JsonObject } from 'types/json';
import { ScenarioDefinition } from 'types/scenario';
import { DataStore } from 'io/data/data-store';
import { parseMapDataSettings } from 'utils/client-utils';
import MapboxMapComponent from 'map/mapbox/mapbox-container';
import Ribbon from 'ui/ribbon/ribbon';
import ScenarioModal from 'ui/modal/scenario';
import FloatingPanelContainer from 'ui/tree/floating-panel';
import { getScenario } from 'state/map-feature-slice';

// Type definition of incoming properties
interface MapContainerProps {
  settings: string;
  data: string;
  scenarios: ScenarioDefinition[]
}

/**
 * Renders the map and its UI components
 */
export default function MapContainer(props: MapContainerProps) {
  const [showDialog, setShowDialog] = useState<boolean>(!!props.scenarios);
  const [mapData, setMapData] = useState<JsonObject>(JSON.parse(props.data));
  const mapSettings: MapSettings = JSON.parse(props.settings);
  const selectedScenario = useSelector(getScenario);

  // Retrieves data settings for specified scenario from the server, else, defaults to the local file
  useEffect(() => {
    if (selectedScenario) {
      // Data will be reset and await the new definitions from the server
      setMapData(null);
      const currentScenario: ScenarioDefinition = props.scenarios.find((scenario) => scenario.id === selectedScenario);
      fetch(`${currentScenario.url}/getDataJson/${selectedScenario}?dataset=${currentScenario.dataset}`)
        .then((res) => res.json())
        .then((data) => {
          setMapData(data);
        });
    }
  }, [showDialog]);

  // On initial start up or user request, scenario dialog will be shown if scenarios are required
  if (showDialog) {
    return (<ScenarioModal
      scenarios={props.scenarios}
      show={showDialog}
      setShowState={setShowDialog}
    />)
  }

  // This code enforces that users must serve the data.json either from the local environment or a server (if scenarios are involved).
  // When retrieving from a server, the data must be loaded before the remaining components renders to circumvent the multitudes of errors.
  if (mapData) {
    const dataStore: DataStore = parseMapDataSettings(mapData, mapSettings?.type);
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
}