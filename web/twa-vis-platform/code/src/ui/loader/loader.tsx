import React from 'react';

import styles from './loader.module.css';

export default function Loader() {
  return <div className={styles.loadingContainer}>
    <img
      src="/img/loading.gif"
      width="500px"
      height="500px"
    />
    <h1>Loading visualisation, please wait...</h1>
  </div>
}