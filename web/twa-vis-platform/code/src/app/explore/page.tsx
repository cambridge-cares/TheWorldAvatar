import React from 'react';

import SettingsStore from 'io/config/settings';
import MapContainer from 'map/map-container';
import { ScenarioDefinition } from 'types/scenario';
import { DefaultSettings } from 'types/settings';

async function getScenarios(scenarioUrl: string): Promise<ScenarioDefinition[]> {
  const url: string = scenarioUrl + "/getScenarios";
  let response;
  try {
    response = await fetch(url);
  } catch (error) {
    console.error(`Failed to fetch scnarios from URL specified in 'ui-settings': ${scenarioUrl}\n`, error);
    return;
  }
  return response.json();
}

/**
 * A server component that handles the explore  route (i.e. "/explore") to display the map container and its components.
 * 
 */
export default async function VisualisationPage() {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  let scenarios: ScenarioDefinition[];
  // When scenarios are available, retrieve their definitions on the server side
  if (uiSettings.resources?.scenario) {
    try {
      scenarios = await getScenarios(uiSettings.resources.scenario.url);
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
      settings={SettingsStore.getMapSettings()}
      data={SettingsStore.getMapDataSettings()}
      scenarios={scenarios}
    />
  )
}