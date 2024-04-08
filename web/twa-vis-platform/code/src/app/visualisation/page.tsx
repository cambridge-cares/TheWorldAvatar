import React from 'react';

import SettingsStore from 'io/config/settings';
import MapContainer from 'map/map-container';

/**
 * Handles the visualisation route (i.e. "/visualisation") to display the map container and its components.
 * 
 */
export default function VisualisationPage() {
  SettingsStore.readMapSettings();
  SettingsStore.readMapDataSettings();
  return (
    <MapContainer
      settings={SettingsStore.getMapSettings()}
      data={SettingsStore.getMapDataSettings()}
    />
  )
}