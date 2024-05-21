import { Icon, Tooltip } from '@mui/material';
import styles from './floating-panel.module.css';

import React, { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Map } from 'mapbox-gl';

import { getIndex, setIndex } from 'state/floating-panel-slice';
import { getQueryTrigger, getIri, getStack, getScenario, setQueryTrigger, getProperties, getFeatures, MapFeaturePayload } from 'state/map-feature-slice';
import { useGetMetadataQuery } from 'utils/server-utils';
import { DataStore } from 'io/data/data-store';
import { MapLayerGroup } from 'types/map-layer';
import { IconSettings, LegendSettings } from 'types/settings';
import LayerTree from './layer/layer-tree';
import LegendTree from './legend/legend-tree';
import InfoTree from './info/info-tree';

// Incoming parameters for component.
interface FloatingPanelContainerProps {
  map: Map;
  dataStore: DataStore;
  icons: IconSettings;
  legend: LegendSettings;
  hideInfo?: boolean;
}

function genQueryEndpoint(iri: string, stack: string, scenario: string): string {
  if (scenario) {
    return `${stack}/CReDoAccessAgent/getMetadataPrivate/${scenario}?iri=${encodeURIComponent(iri)}`;
  }
  return `${stack}/feature-info-agent/get?iri=${encodeURIComponent(iri)}`;
}

/**
 * Floating panel that contains the layer tree and legend components.
 */
export default function FloatingPanelContainer(
  props: Readonly<FloatingPanelContainerProps>
) {
  const [isPanelVisible, setIsPanelVisible] = useState(true);
  const [queriedData, setQueriedData] = useState(null);
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
  const trigger = useSelector(getQueryTrigger);
  const availableFeatures: MapFeaturePayload[] = useSelector(getFeatures);

  const buttonClass = styles.headButton;
  const buttonClassActive = [styles.headButton, styles.active].join(" ");

  // Execute API call
  const { data, isFetching } = useGetMetadataQuery(genQueryEndpoint(selectedIri, selectedStack, selectedScenario), { skip: !trigger });

  // Effect to display additional feature information retrieved from an agent only once it has been loaded
  useEffect(() => {
    if (isFetching) {
      // WIP: Add required functionality while data is still being fetched
    } else {
      // If there is any data retrieved, set that first
      if (data && Object.keys(data).length !== 0) {
        setQueriedData(data);
        dispatch(setQueryTrigger(false));
      } else if (selectedProperties) {
      // Else default to built in data that excludes IRI
        const builtInData = {
          meta: {
            Properties: Object.fromEntries(
              Object.entries(selectedProperties)
                .filter(([key]) => key !== 'iri')
            )
          }
        }
        setQueriedData(builtInData);
      }
    }
  }, [isFetching]);

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
          {activeIndex === 0 && <LayerTree
            map={props.map}
            dataStore={props.dataStore}
            icons={props.icons}
            mapGroups={mapLayerGroups}
            setMapGroups={setMapLayerGroups}
          />}
          {activeIndex === 1 && <LegendTree settings={props.legend} />}
          {activeIndex === 2 &&
            <InfoTree
              data={queriedData}
              isFetching={isFetching}
              activeTab={{
                index: activeInfoTab,
                setActiveTab: setActiveInfoTab,
              }}
              features={availableFeatures}
            />}
        </div>
      )}
    </div>
  );
}
