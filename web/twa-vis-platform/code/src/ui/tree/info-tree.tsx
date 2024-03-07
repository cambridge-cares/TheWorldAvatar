import styles from './info-tree.module.css';
import React, { useEffect, useState } from 'react';
import { useSelector } from 'react-redux';
import { skipToken } from '@reduxjs/toolkit/query/react';

import { DataStore } from 'io/data/data-store';
import {
  getProperties,
  getSourceLayerId,
  getLatLng,
} from 'state/map-feature-slice';
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
  // Execute API call
  const { data, error, isFetching } = useGetMetadataQuery(
    getApiParams() ?? skipToken
  );

  function getApiParams(): ApiParams {
    if (
      !selectedFeatureProperties ||
      !selectedFeatureProperties.iri ||
      !stack
    ) {
      return undefined;
    }
    return {
      iri: selectedFeatureProperties.iri,
      stack: stack,
      scenarioID: scenarioID,
    };
  }

  // Effect to update selected feature's stack endpoint each time a new feature is selected
  useEffect(() => {
    if (selectedFeatureProperties && selectedFeatureProperties.iri) {
      // Retrieving constants
      let stackEndpoint: string =
        props.dataStore.getStackEndpoint(selectedSourceLayer);
      setStack(stackEndpoint);
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
        if (!selectedFeatureProperties || !selectedFeatureProperties.iri) {
          console.warn("IRI is missing. Data fetching will be skipped.");
        } else if (!stack) {
          console.warn(
            "Feature does not have a defined stack. Data fetching will be skipped."
          );
        } else {
          console.error("Error fetching data:", error);
        }
      }
    }
  }, [isFetching]);

  return (
    <div className={styles.infoPanelContainer}>
      <h2>Feature Information</h2>
      {isFetching ? (
        <div className={styles.spinner}>
          <p></p>
          {/* Playholder for adding a loading spinner */}
        </div>
      ) : featureInfo ? (
        <div className={styles.infoSection}>
          {featureInfo.meta && (
            <>
              {featureInfo.meta.Properties.display_order.map((key) => {
                if (key in featureInfo.meta.Properties) {
                  const value = featureInfo.meta.Properties[key];
                  return (
                    <p key={key}>
                      {key}:{" "}
                      <span className={styles.infoValue}>
                        {value.toString()}
                      </span>
                    </p>
                  );
                }
                return null;
              })}
              {Object.entries(featureInfo.meta.Properties).map(
                ([key, value]) => {
                  // Skip if key is 'display_order' or already displayed according to 'display_order'
                  if (
                    !featureInfo.meta.Properties.display_order.includes(key) &&
                    key !== "display_order"
                  ) {
                    return (
                      <p key={key}>
                        {key}:{" "}
                        <span className={styles.infoValue}>
                          {value.toString()}
                        </span>
                      </p>
                    );
                  }
                  return null;
                }
              )}
            </>
          )}
        </div>
      ) : (
        <div className={styles.errorSection}>
          <p>Click to fetch feature information.</p>
        </div>
      )}
    </div>
  );
}
