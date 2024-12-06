import { Icon, Tooltip } from '@mui/material';
import styles from './floating-panel.module.css';

import { Map } from 'mapbox-gl';
import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';

import { DataStore } from 'io/data/data-store';
import { selectDimensionSliderValue } from 'state/dimension-slider-slice';
import { getIndex, setIndex } from 'state/floating-panel-slice';
import { getFeatures, getIri, getProperties, getScenarioID, getStack, MapFeaturePayload } from 'state/map-feature-slice';
import { MapLayerGroup } from 'types/map-layer';
import { IconSettings, LegendSettings } from 'types/settings';
import { ScenarioDimensionsData } from 'types/timeseries';
import DimensionSlider from 'ui/interaction/controls/slider';
import { generateFIAEndpoint, useFeatureInfoAgentService } from 'utils/data-services';
import InfoTree from './info/info-tree';
import LayerTree, { parseIntoTreeStucture } from './layer/layer-tree';
import LegendTree from './legend/legend-tree';

// Incoming parameters for component.
interface FloatingPanelContainerProps {
  map: Map;
  dataStore: DataStore;
  icons: IconSettings;
  legend: LegendSettings;
  scenarioDimensions: ScenarioDimensionsData;
  isDimensionsFetching: boolean;
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
  const selectedScenario = useSelector(getScenarioID);
  const availableFeatures: MapFeaturePayload[] = useSelector(getFeatures);
  //TODO fetch from api
  const buttonClass = styles.headButton;
  const buttonClassActive = [styles.headButton, styles.active].join(" ");
  const dimensionSliderValue = useSelector(selectDimensionSliderValue)
  const hasMultipleDimensions = Object.values(props.scenarioDimensions).some(array => array.length > 1);
  // Execute API call
  const { attributes, timeSeries, isFetching, isUpdating } = useFeatureInfoAgentService(
    generateFIAEndpoint(selectedIri, selectedStack, selectedScenario, ...(hasMultipleDimensions ? [dimensionSliderValue] : [])),
    selectedIri,
    selectedProperties
  );
  // check if scenario dimensions passed down from parent component has multiple dimensions

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
        <button
          className={styles.expandButton}
          onClick={() => setIsPanelVisible(!isPanelVisible)}>
          <Tooltip
            title={isPanelVisible ? "Collapse Panel" : "Expand Panel"}
            enterDelay={500}
            leaveDelay={200}
          >
            <Icon className="material-symbols-outlined">
              {isPanelVisible ? "keyboard_arrow_down" : "keyboard_arrow_up"}
            </Icon>
          </Tooltip>
        </button>
      </div>

      {/* Conditionally render the panel's body */}
      {isPanelVisible && (
        <>
          <div className={styles.floatingPanelBody}>
            {mapLayerGroups.length > 0 && activeIndex === 0 &&
              <LayerTree
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
                featureName={selectedProperties?.name}
                isFetching={isFetching}
                isUpdating={isUpdating}
                activeTab={{
                  index: activeInfoTab,
                  setActiveTab: setActiveInfoTab,
                }}
                features={availableFeatures}
              />}
          </div>

          {!props.isDimensionsFetching && props.scenarioDimensions && hasMultipleDimensions && (
            <div className={styles.floatingPanelControls}>
              <DimensionSlider
                data={props.scenarioDimensions} />
            </div>)}

        </>
      )}
    </div>
  );
}
