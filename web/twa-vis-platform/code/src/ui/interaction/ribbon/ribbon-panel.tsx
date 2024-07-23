import styles from './ribbon.module.css';

import React from 'react';

// Properties for RibbonPanel
interface RibbonPanelProps {
  children?: React.ReactNode
}

/**
 * Single content panel for the visualisation ribbon.
 */
export default function RibbonPanel(props: Readonly<RibbonPanelProps>) {
  return (
    <div className={styles.ribbonPanel}>
      {props.children}
    </div>
  );
}