import styles from "./info-tree.module.css";
import React from "react";
import SVG from "react-inlinesvg";
import { Icon } from "@mui/material";
import { useSelector } from "react-redux";

import { getLatLng } from "../../state/floating-panel-click-slice";

/**
 * Displays a static dummy info component
 */
export default function InfoTree() {
  // Use useSelector to access the latLng state from the Redux store
  const latLng = useSelector(getLatLng);

  return (
    <div className={styles.infoPanelContainer}>
      <h2>Information</h2>

      {/* Display latitude and longitude if available */}
      {latLng && (
        <div className={styles.infoSection}>
          <h3>Clicked Location</h3>
          <p>Latitude: {latLng.lat.toFixed(2)}</p>
          <p>Longitude: {latLng.lng.toFixed(2)}</p>
        </div>
      )}

      <div className={styles.infoSection}>
        <h3>About this Visualization</h3>
        <p>
          This is a placeholder for general information about your map
          visualization. Describe its purpose, data sources, or key concepts.
        </p>
      </div>

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
