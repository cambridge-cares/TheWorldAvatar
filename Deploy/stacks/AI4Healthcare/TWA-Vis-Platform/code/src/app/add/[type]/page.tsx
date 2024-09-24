import React from 'react';
import { Metadata } from 'next';

import { PathNames, PageTitles, Modules } from 'io/config/routes';
import SettingsStore from 'io/config/settings';
import { DefaultSettings } from 'types/settings';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';
import FormContainerComponent from 'ui/interaction/form/form-container';

interface AddFormPageProps {
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
  const metadata: DefaultPageThumbnailProps = uiSettings.links?.find(link => link.url === Modules.REGISTRY);
  return {
    title: metadata?.title ?? PageTitles.REGISTRY,
  }
}

/**
 * Displays the form page for adding an entity.
 */
export default function AddFormPage(props: Readonly<AddFormPageProps>) {
  return (
    <div className="formContainer">
      <FormContainerComponent
        entityType={props.params?.type}
        formType={PathNames.REGISTRY_ADD}
        agentApi={JSON.parse(SettingsStore.getDefaultSettings()).resources?.registry?.url}
      />
    </div>
  );
}