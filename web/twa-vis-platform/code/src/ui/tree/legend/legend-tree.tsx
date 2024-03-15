import styles from './legend-tree.module.css'; // Assuming you have a CSS module for styling
import React from 'react';
import SVG from 'react-inlinesvg';
import MaterialIconButton from 'ui/buttons/icon-button';

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
      <MaterialIconButton
        iconName="timeline"
        classStyles={{
          container: [styles.legendItem],
        }}
        text={{
          styles: [styles.legendText],
          content: "Timeline Representation",
        }}
      />
    </div>
  );
}
