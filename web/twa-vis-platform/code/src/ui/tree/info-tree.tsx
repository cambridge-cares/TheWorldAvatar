import styles from "./info-tree.module.css";
import React, { useEffect, useState } from "react";
import SVG from "react-inlinesvg";
import { Icon } from "@mui/material";
import { useSelector } from "react-redux";
import { getLatLng } from "../../state/floating-panel-click-slice";
import { selectSelectedFeature } from "../../state/map-feature-slice";
import PanelHandler from "../../state/panel-handler-slice";

/**
 * InfoTree component responsible for displaying information about the selected
 * geographic feature and its location. It renders the selected feature's
 * details, such as name, description, and IRI, along with the latitude and
 * longitude of the clicked location. It also fetches and displays additional
 * feature information from an external data source via PanelHandler.
 */
export default function InfoTree() {
  // State to store the latitude and longitude of the clicked location
  const latLng = useSelector(getLatLng);
  // State to store the currently selected feature's information
  const selectedFeature = useSelector(selectSelectedFeature);
  // State to store the modified stack URL
  const [stack, setStack] = useState("");
  // State to store fetched additional information about the selected feature
  const [featureInfo, setFeatureInfo] = useState(null);

  // Effect to fetch and set the stack information on component mount
  useEffect(() => {
    PanelHandler.fetchStack()
      .then(setStack)
      .catch((error) => console.error(error));
  }, []);

  // Effect to fetch additional feature information when a new feature is selected
  useEffect(() => {
    if (selectedFeature && selectedFeature.iri) {
      const scenarioID = "sFCkEoNC"; // Placeholder scenario ID
      PanelHandler.addSupportingData(selectedFeature, scenarioID)
        .then((data) => {
          if (data) {
            setFeatureInfo(data); // Update state with fetched data
          } else {
            // Future Development: Optionally handle the case where no data is returned
          }
        })
        .catch((error) => console.error(error));
    }
  }, [selectedFeature]);

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
      {selectedFeature && (
        <div className={styles.infoSection}>
          <h3>Feature Information</h3>
          <p>Name: {selectedFeature.name}</p>
          <p>Description: {selectedFeature.description}</p>
          <p>IRI: {selectedFeature.iri}</p>
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
