/**
 * Sets a template for all generated HTML files.
 */

import 'ui/css/globals.css';

import React from 'react';
import { ToastContainer } from 'react-toastify';

import StartupLogging from 'io/startup-logging';
import OptionalPages from 'io/config/optional-pages';
import SettingsStore from 'io/config/settings';
import { DefaultSettings } from 'types/settings';
import GlobalContainer from 'ui/global-container';

/**
 * Performs initialisation when the platform is
 * first loaded. Runs on the server.
 */
function initialise() {
    SettingsStore.readInitialisationSettings();
    // Cache contents of optional static pages
    OptionalPages.loadPages();
}

/**
 * Define a root layout template to be used for all generated HTML files.
 * 
 * @param children React child elements to add to generated page.
 * 
 * @returns generated React nodes.
 */
export default function RootLayout({ children, }: { children: React.ReactNode }) {
    // Initialise static content
    initialise();
    // Check if the style-overrides.css file is available
    const styleOverrides: boolean = SettingsStore.hasCssOverrides();

    // Get settings to pass to Toolbar
    const uiSettings: DefaultSettings = JSON.parse(SettingsStore.getDefaultSettings());
    
    // Root element containing all children.
    return (
        <html lang="en">
            <head>
                <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Dosis" />
                <link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,0,0" />
                {
                    styleOverrides &&
                    <link rel="stylesheet" href="/uploads/style-overrides.css" />
                }
            </head>
            <body>
                <StartupLogging/>
                
                <GlobalContainer settings={uiSettings}>
                    {children}
                </GlobalContainer>

                <ToastContainer />
            </body>
        </html>
    );
}