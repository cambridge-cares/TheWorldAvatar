import styles from './info-tree.module.css';
import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import { skipToken } from '@reduxjs/toolkit/query/react';

import { DataStore } from 'io/data/data-store';
import { getProperties, getSourceLayerId, getLatLng } from 'state/map-feature-slice';
import { useGetMetadataQuery, ApiParams } from 'utils/server-utils';

// type definition for incoming properties
type InfoTreeProps = {
  dataStore: DataStore;
};

/**
 * InfoTree component responsible for displaying information about the selected
 * geographic feature and its location. It renders the selected feature's
 * details, such as name, description, and IRI, along with the latitude and
 * longitude of the clicked location. It also fetches and displays additional
 * feature information from an external data source via PanelHandler.
 */
export default function InfoTree(props: InfoTreeProps) {
  // State to store the latitude and longitude of the clicked location
  const latLng = useSelector(getLatLng);
  // State to store the currently selected feature's information
  const selectedFeatureProperties = useSelector(getProperties);
  // State to store the modified stack URL
  const [stack, setStack] = useState("");
  // State to store the currently selected source layer
  const selectedSourceLayer = useSelector(getSourceLayerId);
  // State to store the scenario ID
  const [scenarioID, setScenarioID] = useState("sFCkEoNC");
  // State to store fetched additional information about the selected feature
  const [featureInfo, setFeatureInfo] = useState(null);
  const { data, error, isFetching } = useGetMetadataQuery(getApiParams() ?? skipToken);

  function getApiParams(): ApiParams {
    if (!selectedFeatureProperties || !selectedFeatureProperties.iri) {
      console.error("Feature is missing required information (IRI).");
      return undefined;
    }
    if (stack === undefined) {
      console.error("Feature does not have a defined stack.");
      return undefined;
    }
    return { iri: selectedFeatureProperties.iri, stack: stack, scenarioID: scenarioID };
  }

  // Effect to update selected feature's stack endpoint each time a new feature is selected
  useEffect(() => {
    if (selectedFeatureProperties && selectedFeatureProperties.iri) {
      // Retrieving constants 
      let stackEndpoint: string = props.dataStore.getStackEndpoint(selectedSourceLayer);
      setStack(stackEndpoint);
      console.log(isFetching)
    }
  }, [selectedFeatureProperties]);

  // Effect to display additional feature information retrieved from an agent only once it has been loaded
  useEffect(() => {
    if (isFetching) {
      // WIP: Add required functionality while data is still being fetched
    } else {
      if (data) {
        setFeatureInfo(data); // Update state with fetched data
      } else if (error) {
        console.error("Error fetching data:", error);
      }
    }
  }, [isFetching]);

  return (
    <div className={styles.infoPanelContainer}>
      <h2>Information</h2>
      {latLng && (
        <div className={styles.infoHeadSection}>
          <h3>Clicked Location</h3>
          <p>
            Latitude: {latLng.lat.toFixed(2)} Longitude: {latLng.lng.toFixed(2)}
          </p>
        </div>
      )}
      {selectedFeatureProperties && (
        <div className={styles.infoSection}>
          <h3>Feature Information</h3>
          <p>Name: {selectedFeatureProperties.name}</p>
          <p>Description: {selectedFeatureProperties.description}</p>
          <p>IRI: {selectedFeatureProperties.iri}</p>
        </div>
      )}
      {stack && (
        <div className={styles.infoSection}>
          <h3>Stack Information</h3>
          <p>{stack}</p>
        </div>
      )}
      {featureInfo && (
        <div className={styles.infoSection}>
          <h3>Additional Feature Information</h3>
          {featureInfo.meta &&
            Object.entries(featureInfo.meta.Properties).map(([key, value]) => (
              <p key={key}>
                {key}:{" "}
                <span className={styles.infoValue}>{value.toString()}</span>
              </p>
            ))}
          {/* Iterate over and display other parts of featureInfo as needed */}
        </div>
      )}
    </div>
  );
}
