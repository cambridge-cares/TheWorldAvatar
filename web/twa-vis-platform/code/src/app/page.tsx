/**
 * Handles the default (i.e. "/") route.
 */

import React from 'react';
import { Metadata } from 'next';
import { redirect } from 'next/navigation';

import LandingPage from 'ui/pages/landing';
import OptionalPages from 'io/config/optional-pages';
import { Paths } from 'io/config/routes';
import SettingsStore from 'io/config/settings';
import { UISettings } from 'types/settings';

/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
  const landingPage = OptionalPages.getPage("landing");
  if (landingPage) {
    return {
      title: landingPage.title
    }
  } else {
    return {
      title: "Welcome"
    }
  }
}

/**
 * Handles the default route (i.e. "/") to display a home page
 * or redirect to another page.
 * 
 * @returns JSX for default (home) page.
 */
export default function App() {
  const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());
  if (uiSettings.modules.landing) {
    return (<LandingPage
      settings={uiSettings}
    />);
  } else {
    redirect(Paths.MAP);
  }
}
