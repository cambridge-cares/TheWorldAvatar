import React from 'react';

import styles from './loader.module.css';

export default function LoadingSpinner() {
  return <div className={styles.spinnerContainer}>
    <div className={styles.spinner}></div>
  </div>
}