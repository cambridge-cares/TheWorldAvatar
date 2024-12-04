"use client";

import 'mapbox-gl/dist/mapbox-gl.css';
import styles from './map-container.module.css';

import { FilterSpecification, Map } from 'mapbox-gl';
import { useEffect, useMemo, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { DataStore } from 'io/data/data-store';
import { selectDimensionSliderValue } from 'state/dimension-slider-slice';
import { getFilterFeatureIris, getFilterLayerIds, getFilterTimes, getScenarioID, setFilterFeatureIris, setFilterLayerIds, setFilterTimes } from 'state/map-feature-slice';
import { ScenarioDefinition } from 'types/scenario';
import { CameraPosition, ImageryOption, MapSettings } from 'types/settings';
import ScenarioModal from 'ui/interaction/modal/scenario';
import { SHOW_ALL_FEATURE_INDICATOR } from 'ui/interaction/modal/search/search-modal';
import Ribbon from 'ui/interaction/ribbon/ribbon';
import FloatingPanelContainer from 'ui/interaction/tree/floating-panel';
import MapEventManager from 'ui/map/map-event-manager';
import { addData, getCurrentImageryOption, getDefaultCameraPosition } from 'ui/map/map-helper';
import MapboxMapComponent from 'ui/map/mapbox/mapbox-container';
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
  const [dataStore, setDataStore] = useState<DataStore>(null);

  const selectedScenario = useSelector(getScenarioID);
  const { scenarioDimensions, isDimensionsFetching } = useScenarioDimensionsService(currentScenario?.url, selectedScenario);
  const dimensionSliderValue = useSelector(selectDimensionSliderValue);
  const filterLayerIds: string[] = useSelector(getFilterLayerIds);
  const filterFeatureIris: string[] = useSelector(getFilterFeatureIris);
  const filterTimes: number[] = useSelector(getFilterTimes);

  // Memoizes parsing of settings only once initially to prevent any unintended calls
  const mapSettings: MapSettings = useMemo(() => {
    return JSON.parse(props.settings);
  }, []);

  const defaultPosition: CameraPosition = useMemo(() => {
    return getDefaultCameraPosition(mapSettings.camera);
  }, []);

  const currentImageryOption: ImageryOption = useMemo(() => {
    return getCurrentImageryOption(mapSettings.imagery);
  }, []);

  useEffect(() => {
    if (map) {
      setMapEventManager(new MapEventManager(map));
    }
  }, [map]);

  // Retrieves data settings for specified scenario from the server, else, defaults to the local file
  useEffect(() => {
    if (!showDialog) {
      setDataStore(null); // Always reset data when traversing states
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
            setDataStore(mapDataStore);
          });
      } else {
        // By default, the data settings are retrieved locally
        mapDataStore = parseMapDataSettings(JSON.parse(props.data), mapSettings?.type);
        setDataStore(mapDataStore);
      }
    }
  }, [mapSettings?.type, props.data, props.scenarios, selectedScenario, showDialog, dimensionSliderValue]);

  // Populates the map after it has loaded and scenario selection is not required
  useEffect(() => {
    if (map && dataStore && mapEventManager) {
      if (mapSettings?.["type"] === "mapbox") {
        // All event listeners and data must be added when the map is initialised or data changes
        addData(map, mapSettings, dataStore);
        mapEventManager.addMapboxEventListeners(dispatch, dataStore);

        // When the base imagery is updated, all data layers are removed (point annotations are not removed)
        // This event listener ensures that data layers are reloaded initially and after any style changes
        // The same event listeners can be reused given the same underlying data
        map.on("style.load", function () {
          addData(map, mapSettings, dataStore);
        });
      }
    }
  }, [dispatch, map, dataStore, mapEventManager, mapSettings]);

  // Update the filters for the specific layers if search is required
  useEffect(() => {
    if (map && dataStore && filterLayerIds?.length > 0 && (filterFeatureIris?.length > 0 || filterTimes.length > 0)) {
      // Reset the filters first before applying new filters
      filterLayerIds.map(layerId => map.setFilter(layerId, null));
      // By default, show all feature will have reset filters
      let filter: FilterSpecification = ["all"];
      // The filter settings should only be updated if there is no SHOW_ALL_FEATURE_INDICATOR
      if (filterFeatureIris?.length > 0 && !(filterFeatureIris?.length === 1 && filterFeatureIris[0] === SHOW_ALL_FEATURE_INDICATOR)) {
        // Use exact match to ensure only matching values are shown, do not use in, contains or other expressions with possible substrings
        const valueResultMap: (string | boolean)[] = filterFeatureIris.flatMap(iri => [iri, true]);
        filter.push(["match", ["string", ["get", "iri"]], ...valueResultMap, false]);
      };
      // Add filter times if they exist - start and end period is in first and second position respectively
      if (filterTimes.length != 0) {
        filter.push([
          "case",
          ["has", "time"], // Check if the "time" property exists
          ["all", // If "time" exists, search within the "time" property
            ["<=", filterTimes[0], ["get", "time"]],
            ["<=", ["get", "time"], filterTimes[1]]
          ],
          ["all", // Else if time does not exist, search between the "start" and "end" property
            ["<=", filterTimes[0], ["get", "start"]],
            ["<=", ["get", "end"], filterTimes[1]]
          ],
        ]);
      }
      // If no filters are added, reset it to null
      if (filter.length === 1) {
        filter = null;
      }

      filterLayerIds.map(layerId => map.setFilter(layerId, filter));
      // Reset the filter features after usage
      dispatch(setFilterFeatureIris([]));
      dispatch(setFilterLayerIds([]));
      dispatch(setFilterTimes([]));
    }
  }, [map, dataStore, filterLayerIds, filterFeatureIris, filterTimes]);

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
          currentMap={map}
          styles="mapContainer"
          setMap={setMap}
          defaultPosition={defaultPosition}
          imageryOption={currentImageryOption}
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
        {!showDialog && dataStore && <div className={styles.upperContainer}>
          <FloatingPanelContainer map={map} dataStore={dataStore} icons={mapSettings.icons} legend={mapSettings.legend} scenarioDimensions={scenarioDimensions} isDimensionsFetching={isDimensionsFetching} />
        </div>
        }
        <div className={styles.lowerContainer} />
      </div>
    </>
  )
}