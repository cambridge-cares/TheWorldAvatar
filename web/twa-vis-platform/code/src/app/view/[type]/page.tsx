import React from 'react';
import { Metadata } from 'next';
import { redirect } from 'next/navigation';

import SettingsStore from 'io/config/settings';
import { Paths, PageTitles, Modules } from 'io/config/routes';
import { UISettings } from 'types/settings';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';
import RegistryTableComponent from 'ui/graphic/table/registry/registry-table-component';

interface ViewRegistryPageProps {
  params: {
    type: string
  }
}

/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  const metadata: DefaultPageThumbnailProps = uiSettings.links?.find(link => link.url === Modules.REGISTRY);
  return {
    title: metadata?.title ?? PageTitles.REGISTRY,
  }
}

/**
 * Displays the view page showing a registry of items.
 * 
 * @returns React component for display. 
 */
export default function ViewRegistryPage(props: Readonly<ViewRegistryPageProps>) {
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  // Scheduler API should only be given IF scheduling is enabled in the configuration and current route matches the entity of interest
  const schedulerApi: string = uiSettings.modules.scheduler && uiSettings.resources?.scheduler?.data === props.params?.type
    ? uiSettings.resources?.scheduler?.url : null;

  if (uiSettings.modules.registry && uiSettings.resources?.registry) {
    return (
      <RegistryTableComponent
        entityType={props.params?.type}
        registryAgentApi={uiSettings.resources?.registry?.url}
        schedulerAgentApi={schedulerApi}
      />
    );
  } else {
    redirect(Paths.HOME);
  }
}