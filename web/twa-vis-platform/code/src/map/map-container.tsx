"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './map-container.module.css';

import { Map } from 'mapbox-gl';
import { useEffect, useMemo, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { DataStore } from 'io/data/data-store';
import MapEventManager from 'map/map-event-manager';
import { addData } from 'map/map-helper';
import MapboxMapComponent from 'map/mapbox/mapbox-container';
import { selectDimensionSliderValue } from 'state/dimension-slider-slice';
import { getFilterFeatureIris, getFilterLayerIds, getScenarioID, setFilterFeatureIris, setFilterLayerIds } from 'state/map-feature-slice';
import { ScenarioDefinition } from 'types/scenario';
import { MapSettings } from 'types/settings';
import ScenarioModal from 'ui/interaction/modal/scenario';
import Ribbon from 'ui/interaction/ribbon/ribbon';
import FloatingPanelContainer from 'ui/interaction/tree/floating-panel';
import { parseMapDataSettings } from 'utils/client-utils';
import { useScenarioDimensionsService } from 'utils/data-services';

// Type definition of incoming properties
interface MapContainerProps {
  scenarioURL: string;
  settings: string;
  data: string;
  scenarios: ScenarioDefinition[]
}

/**
 * Renders the map and its UI components
 */
export default function MapContainer(props: MapContainerProps) {
  const dispatch = useDispatch();
  const [map, setMap] = useState<Map>(null);
  const [mapEventManager, setMapEventManager] = useState<MapEventManager>(null);
  const [currentScenario, setCurrentScenario] = useState<ScenarioDefinition>(null);
  const [showDialog, setShowDialog] = useState<boolean>(!!props.scenarios);
  const [mapData, setMapData] = useState<DataStore>(null);

  const selectedScenario = useSelector(getScenarioID);
  const { scenarioDimensions, isDimensionsFetching } = useScenarioDimensionsService(currentScenario?.url, selectedScenario);
  const dimensionSliderValue = useSelector(selectDimensionSliderValue);
  const filterLayerIds: string[] = useSelector(getFilterLayerIds);
  const filterFeatureIris: string[] = useSelector(getFilterFeatureIris);

  // Memoizes parsing of settings only once initially to prevent any unintended calls
  const mapSettings: MapSettings = useMemo(() => {
    return JSON.parse(props.settings);
  }, []);

  // Retrieves data settings for specified scenario from the server, else, defaults to the local file
  useEffect(() => {
    if (!showDialog) {
      setMapData(null); // Always reset data when traversing states
      let mapDataStore: DataStore;
      // If there are any scenarios, the corresponding data settings should be fetched from the server
      if (selectedScenario) {
        // Await the new definitions from the server
        const reqScenario: ScenarioDefinition = props.scenarios.find((scenario) => scenario.id === selectedScenario);
        setCurrentScenario(reqScenario);
        fetch(`${reqScenario.url}/getDataJson/${selectedScenario}?dataset=${reqScenario.dataset}`)
          .then((res) => res.json())
          .then((data) => {
            // Default dimension value is set to 1 unless dimension slider value exists
            let dimensionValue: string = "1";
            if (dimensionSliderValue) {
              dimensionValue = dimensionSliderValue.toString();
            }
            const dataString: string = JSON.stringify(data).replace(/{dim_time_index}/g, dimensionValue);
            mapDataStore = parseMapDataSettings(JSON.parse(dataString), mapSettings?.type);
          });
      } else {
        // By default, the data settings are retrieved locally
        mapDataStore = parseMapDataSettings(JSON.parse(props.data), mapSettings?.type);
      }
      setMapData(mapDataStore);
    }
  }, [mapSettings?.type, props.data, props.scenarios, selectedScenario, showDialog, dimensionSliderValue]);

  // Populates the map after it has loaded and scenario selection is not required
  useEffect(() => {
    if (map && mapData) {
      if (mapSettings?.["type"] === "mapbox") {
        // All event listeners and data must be added when the map is initialised or data changes
        addData(map, mapSettings, mapData);
        mapEventManager.addMapboxEventListeners(dispatch, mapData);

        // When the base imagery is updated, all data layers are removed (point annotations are not removed)
        // This event listener ensures that data layers are reloaded initially and after any style changes
        // The same event listeners can be reused given the same underlying data
        map.on("style.load", function () {
          addData(map, mapSettings, mapData);
        });
      }
    }
  }, [dispatch, map, mapData, mapEventManager, mapSettings]);

  // Update the filters for the specific layers if search is required
  useEffect(() => {
    if (map && mapData && filterLayerIds?.length > 0 && filterFeatureIris?.length > 0) {
      filterLayerIds.map(layerId => map.setFilter(layerId, ["in", ["get", "iri"], ["literal", filterFeatureIris]]));
      // Reset the filter features after usage
      dispatch(setFilterFeatureIris([]));
      dispatch(setFilterLayerIds([]));
    }
  }, [map, mapData, filterLayerIds, filterFeatureIris]);

  return (
    <>
      {/* On initial start up or user request, scenario dialog will be shown if scenarios are required */}
      {showDialog &&
        <ScenarioModal
          scenarioURL={props.scenarioURL}
          scenarios={props.scenarios}
          show={showDialog}
          setShowState={setShowDialog}
        />}

      {/* Mapbox map */}
      {mapSettings?.["type"] === "mapbox" &&
        <MapboxMapComponent
          settings={mapSettings}
          currentMap={map}
          setMap={setMap}
          setMapEventManager={setMapEventManager}
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
          toggleScenarioSelection={setShowDialog}
          hasScenario={!!selectedScenario}
        />

        {/* Map information panel */}
        {!showDialog && mapData && <div className={styles.upperContainer}>
          <FloatingPanelContainer map={map} dataStore={mapData} icons={mapSettings.icons} legend={mapSettings.legend} scenarioDimensions={scenarioDimensions} isDimensionsFetching={isDimensionsFetching} />
        </div>
        }
        <div className={styles.lowerContainer} />
      </div>
    </>
  )
}