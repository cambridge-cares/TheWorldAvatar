import styles from './decagon.module.css';

import React from 'react';

interface DecagonIconComponentProps {
  readonly color: string;
  readonly classes?: string;
}

/**
 * Reusable decagon icon component.
 * 
 * @param {string} color The color of the decagon.
 * @param {string} classes Additional CSS classes to apply to the icon element.
 */
export default function DecagonIconComponent(props: DecagonIconComponentProps) {
  return (
    <div className={`${styles.container} ${props.classes}`}>
      <div className={`${styles.decagon} ${styles.start}`} style={{ backgroundColor: props.color }}></div>
      <div className={`${styles.decagon} ${styles.middle}`} style={{ backgroundColor: props.color }}></div>
      <div className={`${styles.decagon} ${styles.end}`} style={{ backgroundColor: props.color }}></div>
    </div>);
}