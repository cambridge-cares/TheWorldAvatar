
import { Metadata } from 'next';
import { redirect } from 'next/navigation';

import SettingsStore from 'io/config/settings';
import { Paths, PageTitles, Modules } from 'io/config/routes';
import MapContainer from 'map/map-container';
import { ScenarioDefinition } from 'types/scenario';
import { DefaultSettings } from 'types/settings';
import { getScenarios } from 'utils/getScenarios';
import { DefaultPageThumbnailProps } from 'ui/pages/page-thumbnail';

export const dynamic = 'force-dynamic';


/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  const metadata: DefaultPageThumbnailProps = uiSettings.links?.find(link => link.url === Modules.MAP);
  return {
    title: metadata?.title ?? PageTitles.MAP,
  }
}

/**
 * A server component that handles the explore  route (i.e. images/defaultsexplore") to display the map container and its components.
 * 
 */
export default async function VisualisationPage() {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  if (uiSettings.modules.map) {
    let scenarios: ScenarioDefinition[];
    // When scenarios are available, retrieve their definitions on the server side
    if (uiSettings.resources?.scenario) {
      try {
        scenarios = await getScenarios(uiSettings.resources?.scenario?.url);
        scenarios = scenarios.map((scenario) => ({
          ...scenario,
          url: uiSettings.resources.scenario.url,
          dataset: uiSettings.resources.scenario.data,
        }))
      } catch (error) {
        console.error(`Error populating scenarios selector`, error)
      }
    }

    SettingsStore.readMapSettings();
    await SettingsStore.readMapDataSettings();

    return (
      <MapContainer
        scenarioURL={SettingsStore.getDefaultSettings()}
        settings={SettingsStore.getMapSettings()}
        data={SettingsStore.getMapDataSettings()}
        scenarios={scenarios}
      />
    )
  } else {
    redirect(Paths.HOME);
  }
}