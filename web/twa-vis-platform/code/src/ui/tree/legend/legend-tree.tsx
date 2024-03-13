import styles from './legend-tree.module.css'; // Assuming you have a CSS module for styling
import React from 'react';
import SVG from 'react-inlinesvg';
import { Icon } from '@mui/material';

/**
 * Displays a static dummy legend component
 */
export default function LegendTree() {
  return (
    <div className={styles.legendContainer}>
      <h2>Legend</h2>

      {/* Sample Legend Items */}
      <div className={styles.legendItem}>
        <SVG
          src="/uploads/images/icons/catherine.png"
          className={styles.legendIcon}
        />
        <span className={styles.legendText}>Layer Type 1</span>
      </div>

      <div className={styles.legendItem}>
        <div
          className={styles.legendColorBlock}
          style={{ backgroundColor: "red" }}
        ></div>
        <span className={styles.legendText}>Data Category A</span>
      </div>

      <div className={styles.legendItem}>
        <Icon className="material-symbols-outlined">timeline</Icon>
        <span className={styles.legendText}>Timeline Representation</span>
      </div>
    </div>
  );
}
