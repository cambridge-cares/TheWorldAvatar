import React from 'react';

import styles from './loader.module.css';
import AppImage from 'ui/graphic/image/image';

export default function Loader() {
  return <div className={styles.loadingContainer}>
    <AppImage url="../../images/defaults/loading.gif"
      width={500}
      height={500} 
      alt ="Loading animation"/>
    <h1>Loading, please wait...</h1>
  </div>
}