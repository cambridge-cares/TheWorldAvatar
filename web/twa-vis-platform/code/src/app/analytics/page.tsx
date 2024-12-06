import styles from './analytics.module.css';

import React from 'react';
import { Metadata } from 'next';
import { redirect } from 'next/navigation';

import SettingsStore from 'io/config/settings';
import { Paths, PageTitles, Modules } from 'io/config/routes';
import { UISettings } from 'types/settings';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';


/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  const metadata: DefaultPageThumbnailProps = uiSettings.links?.find(link => link.url === Modules.DASHBOARD);
  return {
    title: metadata?.title ?? PageTitles.DASHBOARD,
  }
}

/**
 * A page displaying the dashboard.
 * 
 * @returns React component for display. 
 */
export default function DashContainer() {
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  if (uiSettings.resources?.dashboard?.url) {
    return (
      <div className={styles.dashContainer}>
        <iframe className={styles.dashboard} src={uiSettings.resources.dashboard.url} title="Dashboard"></iframe>
      </div>
    )
  } else {
    redirect(Paths.HOME);
  }
}