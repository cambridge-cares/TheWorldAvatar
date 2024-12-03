import React from 'react';
import { Metadata } from 'next';

import SettingsStore from 'io/config/settings';
import { Modules, PageTitles, Paths } from 'io/config/routes';
import { UISettings } from 'types/settings';
import FormModal from 'ui/interaction/modal/form/form-modal';
import FormContainerComponent from 'ui/interaction/form/form-container';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';

interface InterceptAddFormPageProps {
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
  };
}

/**
 * Displays the intercepted route for adding an entity through a modal.
 */
export default async function InterceptAddFormPage(props: Readonly<InterceptAddFormPageProps>) {
  const { params } = props;
  const resolvedParams = await params;
  return (
    <FormModal>
      <FormContainerComponent
        entityType={resolvedParams.type}
        formType={Paths.REGISTRY_ADD}
        agentApi={JSON.parse(SettingsStore.getDefaultSettings()).resources?.registry?.url}
      />
    </FormModal>
  );
}
