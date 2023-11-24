import path from "path";
import fs from "fs";

import "./css/globals.css"
import StartupLogging from "@/utils/startup-logging";
import UISettings from "@/utils/settings/ui-settings";
import { OptionalPages } from "@/utils/settings/optional-pages";
import Toolbar from "@/components/toolbar/toolbar";

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
 * 
 * @param param0 
 * @returns 
 */
export default function RootLayout({ children, }: { children: React.ReactNode }) {
    // Initialise static content
    initialise();
    
    // Check if the style-overrides.css file is available
    let styleOverrides = hasCSSOverrides();

    // Get settings to pass to Toolbar
    let moduleSettings = UISettings.getModuleSettings();
    let brandingSettings = UISettings.getBrandingSettings();
    
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

                <div id="globalContainer">
                    {/* Slim toolbar component */}
                    <Toolbar
                        landing={moduleSettings.landing}
                        help={moduleSettings.help}
                        dashboard={moduleSettings.dashboard}
                        toolbarLogo={brandingSettings.toolbarLogo.toString()}
                    />

                    <div id="contentContainer">
                        {children}
                    </div>
                </div>
            </body>
        </html>
    );
}

/**
 * Returns true if the "style-overrides.css" file exists within
 * the hosted "uploads" directory.
 */
function hasCSSOverrides() {
    let url = path.join(process.cwd(), "../uploads/style-overrides.css");
    return fs.existsSync(url);
}