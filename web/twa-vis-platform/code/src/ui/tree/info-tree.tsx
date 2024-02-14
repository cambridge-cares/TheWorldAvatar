import styles from "./info-tree.module.css";
import React from "react";
import SVG from "react-inlinesvg";
import { Icon } from "@mui/material";

/**
 * Displays a static dummy info component
 */
export default function InfoTree() {
  return (
    <div className={styles.infoPanelContainer}>
      <h2>Information</h2>

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
