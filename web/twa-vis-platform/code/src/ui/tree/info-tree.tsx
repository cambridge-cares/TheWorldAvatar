import styles from "./info-tree.module.css";
import React from "react";
import SVG from "react-inlinesvg";
import { Icon } from "@mui/material";
import { useSelector } from "react-redux";
import { getLatLng } from "../../state/floating-panel-click-slice";
import { selectSelectedFeature } from "../../state/map-feature-slice";

export default function InfoTree() {
  const latLng = useSelector(getLatLng);
  const selectedFeature = useSelector(selectSelectedFeature); // Use useSelector here

  return (
    <div className={styles.infoPanelContainer}>
      <h2>Information</h2>

      {latLng && (
        <div className={styles.infoSection}>
          <h3>Clicked Location</h3>
          <p>Latitude: {latLng.lat.toFixed(2)}</p>
          <p>Longitude: {latLng.lng.toFixed(2)}</p>
        </div>
      )}

      {selectedFeature && ( // Ensure selectedFeature is not null or undefined
        <div className={styles.infoSection}>
          <h3>Feature Information</h3>
          <p>Name: {selectedFeature.name}</p> {/* Correctly display the name */}
          <p>Description: {selectedFeature.description}</p>{" "}
          {/* Display the description */}
        </div>
      )}

      <div className={styles.infoSection}>
        <h3>How to Use</h3>
        <ul>
          <li>Basic interaction 1</li>
          <li>Basic interaction 2</li>
          <li>Additional instructions</li>
        </ul>
      </div>
    </div>
  );
}
