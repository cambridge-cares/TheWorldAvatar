/**
 * Sets a template for all generated HTML files.
 */

import "ui/css/globals.css"

import fs from "fs";
import path from "path";
import React from "react";

import UISettings from "io/config/ui-settings";
import StartupLogging from "io/startup-logging";
import { OptionalPages } from "io/config/optional-pages";
import GlobalContainer from "../ui/global-container";
import { ToastContainer } from "react-toastify";

/**
 * Performs initialisation when the platform is
 * first loaded. Should run on the server.
 */
function initialise() {
    // Read the UI settings
    UISettings.readSettings();

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
    const styleOverrides = hasCSSOverrides();

    // Get settings to pass to Toolbar
    const uiSettings = UISettings.getSettings();
    
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

/**
 * Returns true if the "style-overrides.css" file exists within
 * the hosted "uploads" directory.
 */
function hasCSSOverrides() {
    const url = path.join(process.cwd(), "../uploads/style-overrides.css");
    return fs.existsSync(url);
}