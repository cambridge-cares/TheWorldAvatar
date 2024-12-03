import React from 'react';
import { Metadata } from 'next';

import { Paths, PageTitles, Modules } from 'io/config/routes';
import SettingsStore from 'io/config/settings';
import { UISettings } from 'types/settings';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';
import FormContainerComponent from 'ui/interaction/form/form-container';

interface EditFormPageProps {
  params: {
    type: string,
    id: string,
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
 * Displays the form page for editing one item.
 * 
 * @returns React component for display. 
 */
export default function EditFormPage(props: Readonly<EditFormPageProps>) {
  return (
    <div className="formContainer">
      <FormContainerComponent
        entityType={props.params?.type}
        formType={Paths.REGISTRY_EDIT}
        agentApi={JSON.parse(SettingsStore.getDefaultSettings()).resources?.registry?.url}
      />
    </div>
  );
}