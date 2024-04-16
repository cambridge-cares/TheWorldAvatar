"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './map-container.module.css';

import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Map } from 'mapbox-gl';

import { MapSettings } from 'types/settings';
import { ScenarioDefinition } from 'types/scenario';
import { DataStore } from 'io/data/data-store';
import { parseMapDataSettings } from 'utils/client-utils';
import { MapboxMapComponent } from 'map/mapbox/mapbox-container';
import Ribbon from 'ui/interaction/ribbon/ribbon';
import ScenarioModal from 'ui/interaction/modal/scenario';
import FloatingPanelContainer from 'ui/interaction/tree/floating-panel';
import { getScenario } from 'state/map-feature-slice';
import { addMapboxEventListeners } from './event-listeners';
import { addIcons } from './mapbox/mapbox-icon-loader';
import { addAllSources } from './mapbox/mapbox-source-utils';
import { addAllLayers } from './mapbox/mapbox-layer-utils';
import { getIsStyleLoaded } from 'state/floating-panel-slice';

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
  const map = useRef<Map>(null);
  const dispatch = useDispatch();
  const [showDialog, setShowDialog] = useState<boolean>(!!props.scenarios);
  const [mapData, setMapData] = useState<DataStore>(null);
  const mapSettings: MapSettings = JSON.parse(props.settings);
  const selectedScenario = useSelector(getScenario);
  const isStyleLoaded: boolean = useSelector(getIsStyleLoaded);

  // Retrieves data settings for specified scenario from the server, else, defaults to the local file
  useEffect(() => {
    setMapData(null); // Always reset data when traversing states
    if (selectedScenario) {
      // Await the new definitions from the server
      const currentScenario: ScenarioDefinition = props.scenarios.find((scenario) => scenario.id === selectedScenario);
      fetch(`${currentScenario.url}/getDataJson/${selectedScenario}?dataset=${currentScenario.dataset}`)
        .then((res) => res.json())
        .then((data) => {
          setMapData(parseMapDataSettings(data, mapSettings?.type));
        });
    } else {
      setMapData(parseMapDataSettings(JSON.parse(props.data), mapSettings?.type));
    }
  }, [showDialog]);

  // Populates the map after it has loaded and scenario selection is not required
  useEffect(() => {
    if (map.current && isStyleLoaded && !showDialog) {
      if (mapSettings?.["type"] === "mapbox") {
        map.current.on("load", function () {
          // Add all map event listeners
          addMapboxEventListeners(map.current, dispatch, mapData);

          // Parse data configuration and load icons
          const iconPromise = addIcons(map, mapSettings.icons);

          Promise.all([iconPromise]).then(() => {
            // Once that is done and completed...
            console.log("Data definitions fetched and parsed.");

            // Plot data
            addAllSources(map, mapData);
            addAllLayers(map, mapData, mapSettings.imagery);
          });
        });
      }
    }
  }, [isStyleLoaded, mapData]);

  return (
    <>
      {/* On initial start up or user request, scenario dialog will be shown if scenarios are required */}
      {showDialog &&
        <ScenarioModal
          scenarios={props.scenarios}
          show={showDialog}
          setShowState={setShowDialog}
        />}

      {/* Mapbox map */}
      {mapSettings?.["type"] === "mapbox" &&
        <MapboxMapComponent
          settings={mapSettings}
          ref={map}
        />
      }

      {/* Cesium map */}
      {mapSettings?.["type"] === "cesium" &&
        <div></div>
      }

      {/* Container elements */}
      <div className={styles.componentContainer}>
        {/* Map controls ribbon */}
        <Ribbon
          map={map}
          startingIndex={0}
          mapSettings={mapSettings}
          hasScenario={!!selectedScenario}
          toggleScenarioSelection={setShowDialog}
        />

        {/* Map information panel */}
        {!showDialog && mapData && <div className={styles.upperContainer}>
          <FloatingPanelContainer map={map} dataStore={mapData} icons={mapSettings.icons} legend={mapSettings.legend} />
        </div>
        }
        <div className={styles.lowerContainer} />
      </div>
    </>
  )
}