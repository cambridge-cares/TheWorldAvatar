import React from 'react';
import { Metadata } from 'next';
import { redirect } from 'next/navigation';

import SettingsStore from 'io/config/settings';
import { PathNames } from 'io/config/routes';
import { DefaultSettings } from 'types/settings';
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
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  const metadata: DefaultPageThumbnailProps = uiSettings.links?.find(link => link.url === "registry");
  return {
    title: metadata?.title ?? "Registry",
  }
}

/**
 * Displays the view page showing a registry of items.
 * 
 * @returns React component for display. 
 */
export default function ViewRegistryPage(props: Readonly<ViewRegistryPageProps>) {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  if (uiSettings.modules.registry && uiSettings.resources?.registry) {
    return (
      <RegistryTableComponent
        entityType={props.params?.type}
        agentApi={uiSettings.resources?.registry?.data}
      />
    );
  } else {
    redirect(PathNames.HOME);
  }
}