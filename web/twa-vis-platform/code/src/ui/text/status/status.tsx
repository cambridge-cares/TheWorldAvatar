import styles from './status.module.css';

import React from 'react';

interface StatusComponentProps<> {
  status: string;
}

/**
 * Renders the status with a circle indicator.
 * 
 * @param {string} status The status to display.
 */
export default function StatusComponent(props: Readonly<StatusComponentProps>) {
  let statusColor: string;

  switch (props.status.toLowerCase()) {
    case "available":
    case "completed":
      statusColor = "#52B7A5";
      break;
    case "unavailable":
    case "cancelled":
    case "incomplete":
    case "rescinded":
    case "terminated":
      statusColor = "#D7653D";
      break;
    default:
      statusColor = "#666";
  }

  return (
    <span className={styles.container}>
      <span className={styles.circle} style={{ borderColor: statusColor }}></span>
      <p className={styles.text} style={{ color: statusColor }}>{props.status}</p>
    </span>
  );
}