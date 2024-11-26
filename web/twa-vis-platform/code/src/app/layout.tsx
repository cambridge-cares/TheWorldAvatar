/**
 * Sets a template for all generated HTML files.
 */

import 'ui/css/globals.css';

import React from 'react';
import { ToastContainer } from 'react-toastify';
import { Dosis } from 'next/font/google';

import StartupLogging from 'io/startup-logging';
import OptionalPages from 'io/config/optional-pages';
import SettingsStore from 'io/config/settings';
import { UISettings } from 'types/settings';
import GlobalContainer from 'ui/global-container';
import BackgroundImage from 'ui/graphic/image/background';

/**
 * Performs initialisation when the platform is
 * first loaded. Runs on the server.
 */
function initialise() {
    SettingsStore.readInitialisationSettings();
    // Cache contents of optional static pages
    OptionalPages.loadPages();
}

const dosis = Dosis({
    subsets: ['latin'],
    display: 'swap',
  })

/**
 * Define a root layout template to be used for all generated HTML files.
 * 
 * @param children React child elements to add to generated page.
 * 
 * @returns generated React nodes.
 */
export default function RootLayout({ children, modal }: { children: React.ReactNode; modal: React.ReactNode; }) {
    // Initialise static content
    initialise();

    // Get settings to pass to Toolbar
    const uiSettings: UISettings = JSON.parse(SettingsStore.getDefaultSettings());

    // Root element containing all children.
    return (
        <html lang="en" className={dosis.className}>
            <body>
                <StartupLogging />
                <GlobalContainer settings={uiSettings}>
                    <BackgroundImage />
                    {children}
                    {modal}
                </GlobalContainer>
                <ToastContainer />
            </body>
        </html>
    );
}