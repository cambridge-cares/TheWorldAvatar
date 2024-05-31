import styles from './info-tree.module.css';

import React from 'react';
import { useDispatch } from 'react-redux';

import { TimeSeriesGroup } from 'types/timeseries';
import { AttributeGroup } from 'types/attribute';
import { MapFeaturePayload } from 'state/map-feature-slice';
import FeatureSelector from 'ui/interaction/dropdown/feature-selector';
import { setSelectedFeature } from 'utils/client-utils';
import AttributeRoot from './attribute-root';
import InfoTabs from './info-tabs';
import TimeSeriesPanel from './time-series-panel';

interface InfoTreeProps {
  attributes: AttributeGroup;
  timeSeries: TimeSeriesGroup;
  isFetching: boolean;
  isUpdating: boolean;
  activeTab: {
    index: number;
    setActiveTab: React.Dispatch<React.SetStateAction<number>>;
  };
  features: MapFeaturePayload[];
}

/**
 * This component is responsible for displaying information about the selected geographic feature 
 * such as name, description, and IRI. Data is passed from the parent component so that 
 * the existing state is persisted even if this component is removed.
 * 
 * @param {AttributeGroup} attributes The processed attributes for user interaction.
 * @param {TimeSeriesGroup} timeSeries The processed time series for user interaction.
 * @param {boolean} isFetching An indicator if the query is still running.
 * @param {MapFeaturePayload[]} features A list of selected features.
 */
export default function InfoTree(props: Readonly<InfoTreeProps>) {
  const dispatch = useDispatch();

  // A function that renders the required contents for this panel
  const renderPanelContents: () => React.ReactElement = () => {
    // Render loading spinner if it is still fetching data
    if (props.isFetching || props.isUpdating) {
      return <div className={styles.spinner}></div>;
    }

    // If there are multiple features clicked, activate feature selector to choose only one
    if (props.features.length > 1) {
      return <FeatureSelector features={props.features} />;
    } else if (props.features.length === 1) {
      // When only feature is available, set its properties
      setSelectedFeature(props.features[0], dispatch);
    }
    // If active tab is 0, render the Metadata Tree
    if (props.attributes && props.activeTab.index === 0) {
      return <AttributeRoot attribute={props.attributes} />;
    }

    if (props.timeSeries && props.activeTab.index > 0) {
      return (
        <TimeSeriesPanel data={props.timeSeries} />
      );
    }
    // Placeholder text when there are no initial data or selected feature
    return <p>Click to fetch feature information.</p>;
  }

  return (
    <div className={styles.infoPanelContainer}>
      <div className={styles.infoHeadSection}>
        <h2>Feature Information</h2>
        {// Display the tabs only if there are both meta and time
          !props.isFetching && props.attributes && props.timeSeries && (
            <InfoTabs
              tabs={{
                hasAttributes: !!props.attributes,
                hasTimeSeries: !!props.timeSeries,
              }}
              activeTab={{
                index: props.activeTab.index,
                setActiveTab: props.activeTab.setActiveTab,
              }}
            />)}
      </div>
      <div className={styles.infoSection}>
        {renderPanelContents()}
      </div>
    </div>
  );
}