import React from 'react';

import SettingsStore from 'io/config/settings';
import MapContainer from 'map/map-container';
import { ScenarioDefinition } from 'types/scenario';
import { DefaultSettings } from 'types/settings';

async function getScenarios(scenarioUrl: string): Promise<ScenarioDefinition[]> {
  let url: string = scenarioUrl;
  if (scenarioUrl) {
    url += "/getScenarios";
    // Fetch data from external API
    const res = await fetch(url);
    if (!res.ok) {
      // This will activate the closest `error.js` Error Boundary
      throw new Error('Failed to fetch data')
    }
    return res.json();
  }
  return null; // Returns null without a scenario url
}

/**
 * A server component that handles the visualisation route (i.e. "/visualisation") to display the map container and its components.
 * 
 */
export default async function VisualisationPage() {
  const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
  let scenarios: ScenarioDefinition[];
  // When scenarios are available, retrieve their definitions on the server side
  if (uiSettings.resources.scenario) {
    scenarios = await getScenarios(uiSettings.resources.scenario.url);
    scenarios = scenarios.map((scenario) => ({
      ...scenario,
      url: uiSettings.resources.scenario.url,
      dataset: uiSettings.resources.scenario.data,
    }))
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