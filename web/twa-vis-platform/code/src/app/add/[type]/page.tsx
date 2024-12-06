import React from 'react';
import { Metadata } from 'next';

import { Paths, PageTitles, Modules } from 'io/config/routes';
import SettingsStore from 'io/config/settings';
import { UISettings } from 'types/settings';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';
import FormContainerComponent from 'ui/interaction/form/form-container';

interface AddFormPageProps {
  params: Promise<{
    type: string
  }>
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
 * Displays the form page for adding an entity.
 */
export default async function AddFormPage(props: Readonly<AddFormPageProps>) {
  const resolvedParams = await props.params;
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  return (
    <div className="formContainer">
      <FormContainerComponent
        entityType={resolvedParams?.type}
        formType={Paths.REGISTRY_ADD}
        agentApi={uiSettings?.resources?.registry?.url}
        isPrimaryEntity={uiSettings?.resources?.registry?.data === resolvedParams?.type}
      />
    </div>
  );
}