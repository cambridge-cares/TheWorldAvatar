import styles from './analytics.module.css';
import React from 'react';

import SettingsStore from 'io/config/settings';
import { DefaultSettings } from 'types/settings';

/**
 * A page displaying the dashboard.
 * 
 * @returns React component for display. 
 */
export default function DashContainer() {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  if (uiSettings.resources?.dashboard?.url) {
    return (
      <div className={styles.dashContainer}>
        <iframe className={styles.dashboard} src={uiSettings.resources.dashboard.url} title="Dashboard"></iframe>
      </div>
    )
  }
}