/**
 * Handles the default (i.e. "/") route.
 */

import React from 'react';
import { Metadata } from 'next';

import LandingPage from 'ui/pages/landing';
import OptionalPages from 'io/config/optional-pages';
import VisualisationPage from './visualisation/page';

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
  const landingPage = OptionalPages.getPage("landing");
  return (
    <>
      {landingPage &&
        <LandingPage />
      }
      {!landingPage &&
        <VisualisationPage />
      }
    </>
  );
}
