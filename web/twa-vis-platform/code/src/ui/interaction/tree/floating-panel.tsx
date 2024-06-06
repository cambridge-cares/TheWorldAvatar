import { Icon, Tooltip } from '@mui/material';
import styles from './floating-panel.module.css';

import { Map } from 'mapbox-gl';
import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { DataStore } from 'io/data/data-store';
import { getIndex, setIndex } from 'state/floating-panel-slice';
import { getFeatures, getIri, getProperties, getScenario, getStack, MapFeaturePayload } from 'state/map-feature-slice';
import { MapLayerGroup } from 'types/map-layer';
import { DefaultSettings, IconSettings, LegendSettings } from 'types/settings';
import { genFIAEndpoint, useFeatureInfoAgentService } from 'utils/data-services';
import { ScenarioDimensionsData, ScenarioDimensionStep } from '../../../types/timeseries';
import DiscreteSlider from '../controls/slider';
import InfoTree from './info/info-tree';
import LayerTree, { parseIntoTreeStucture } from './layer/layer-tree';
import LegendTree from './legend/legend-tree';
import SettingsStore from '../../../io/config/settings';

// Incoming parameters for component.
interface FloatingPanelContainerProps {
  map: Map;
  dataStore: DataStore;
  icons: IconSettings;
  legend: LegendSettings;
  hideInfo?: boolean;
}

/**
 * Floating panel that contains the layer tree and legend components.
 */
export default function FloatingPanelContainer(
  props: Readonly<FloatingPanelContainerProps>
) {
  const [isPanelVisible, setIsPanelVisible] = useState(true);
  const [activeInfoTab, setActiveInfoTab] = React.useState(0);
  const [mapLayerGroups, setMapLayerGroups] = useState<MapLayerGroup[]>([]);

  const showLegend: boolean = !!props.legend;
  const showInfo = props.hideInfo == null || !props.hideInfo;

  const dispatch = useDispatch();
  const activeIndex = useSelector(getIndex);
  const selectedIri = useSelector(getIri);
  const selectedProperties = useSelector(getProperties);
  const selectedStack = useSelector(getStack);
  const selectedScenario = useSelector(getScenario);
  const availableFeatures: MapFeaturePayload[] = useSelector(getFeatures);
  //TODO fetch from api
  const sliderValues: ScenarioDimensionStep[] = [{ "value": 1, "label": "2050/12" }, { "value": 2, "label": "2051/01" }, { "value": 3, "label": "2051/02" }, { "value": 4, "label": "2051/03" }, { "value": 5, "label": "2051/04" }, { "value": 6, "label": "2051/05" }, { "value": 7, "label": "2051/06" }, { "value": 8, "label": "2051/07" }, { "value": 9, "label": "2051/08" }, { "value": 10, "label": "2051/09" }, { "value": 11, "label": "2051/10" }, { "value": 12, "label": "2051/11" }]
  const buttonClass = styles.headButton;
  const buttonClassActive = [styles.headButton, styles.active].join(" ");
  // Execute API call
  const { attributes, timeSeries, isFetching, isUpdating } = useFeatureInfoAgentService(
    genFIAEndpoint(selectedIri, selectedStack, selectedScenario),
    selectedIri,
    selectedProperties
  );

  // useEffect(() => {
  //   const fetchScenarioDimensions = async (scenarioUrl: string) => {
  //     if (selectedScenario) {
  //       try {
          
  //         const response = await fetch(`${scenarioUrl}/getScenarioTimes/${selectedScenario}`);
  //         const data = await response.json();
  //         // Do something with the data
  //       } catch (error) {
  //         console.error('Error fetching times from CentralStackAgent/getScenarioTimes:', error);
  //       }
  //     }
  //   };
    
  //   const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  //   fetchScenarioDimensions(uiSettings.resources.scenario.url);
  // }, [selectedScenario]); // Add dependencies in the dependency array

  useEffect(() => {
    parseIntoTreeStucture(props.dataStore, props.icons, setMapLayerGroups);
  }, [props.dataStore, props.icons])

  const clickAction = (index: number) => {
    dispatch(
      setIndex({
        index: index,
      })
    );
  };

  return (
    <div className={styles.floatingPanelContainer}>
      <div className={styles.floatingPanelHead}>
        {/* Layer tree button */}
        <button
          className={activeIndex == 0 ? buttonClassActive : buttonClass}
          onClick={() => clickAction(0)}
        >
          <Tooltip
            title="Layer Selection"
            enterDelay={1000}
            leaveDelay={100}
            placement="bottom-start"
          >
            <Icon className="material-symbols-outlined">stacks</Icon>
          </Tooltip>
        </button>

        {/* Legend button */}
        {showLegend && (
          <button
            className={activeIndex == 1 ? buttonClassActive : buttonClass}
            onClick={() => clickAction(1)}
          >
            <Tooltip
              title="Key/Legend"
              enterDelay={1000}
              leaveDelay={100}
              placement="bottom-start"
            >
              <Icon className="material-symbols-outlined">key_vertical</Icon>
            </Tooltip>
          </button>
        )}

        {/* Info button */}
        {showInfo && (
          <button
            className={activeIndex == 2 ? buttonClassActive : buttonClass}
            onClick={() => clickAction(2)}
          >
            <Tooltip
              title="Information"
              enterDelay={1000}
              leaveDelay={100}
              placement="bottom-start"
            >
              <Icon className="material-symbols-outlined">info</Icon>
            </Tooltip>
          </button>
        )}

        {/* Toggle visibility button */}
        <button onClick={() => setIsPanelVisible(!isPanelVisible)}>
          <Tooltip
            title={isPanelVisible ? "Collapse Panel" : "Expand Panel"}
            enterDelay={500}
            leaveDelay={200}
          >
            <Icon className="material-symbols-outlined">
              {isPanelVisible ? "arrow_drop_up" : "arrow_drop_down"}
            </Icon>
          </Tooltip>
        </button>
      </div>

      {/* Conditionally render the panel's body */}
      {isPanelVisible && (
        <div className={styles.floatingPanelBody}>
          {mapLayerGroups.length > 0 && activeIndex === 0 && <LayerTree
            map={props.map}
            dataStore={props.dataStore}
            icons={props.icons}
            mapGroups={mapLayerGroups}
            setMapGroups={setMapLayerGroups}
          />}
          {activeIndex === 1 && <LegendTree settings={props.legend} />}
          {activeIndex === 2 &&
            <InfoTree
              attributes={attributes}
              timeSeries={timeSeries}
              isFetching={isFetching}
              isUpdating={isUpdating}
              activeTab={{
                index: activeInfoTab,
                setActiveTab: setActiveInfoTab,
              }}
              features={availableFeatures}
            />}
          <div className={styles.floatingPanelControls}>
            <DiscreteSlider 
            values={sliderValues} />
          </div>
        </div>

      )}
    </div>
  );
}
