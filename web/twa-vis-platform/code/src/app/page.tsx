import React from "react";
import { Metadata } from "next";

import LandingPage from "ui/pages/landing";
import UISettings from "io/config/ui-settings";
import { OptionalPages } from "io/config/optional-pages";

/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
    const moduleSettings = UISettings.getSettings().modules;

    if(moduleSettings.landing) {
        const landingPage = OptionalPages.getPage("landing");
        return {
            title: landingPage.title
        }
    }

    return {
        title: "Welcome"
    }
}


/**
 * Handles the default route (i.e. "/") to display a home page
 * or redirect to another page.
 * 
 * @returns JSX for default (home) page.
 */
export default function App() {
    const moduleSettings = UISettings.getSettings().modules;
    
    if(moduleSettings.landing) {
        // Enabled, load components for that here
        return <LandingPage/>
        
    } else {
        // Disabled, load straight into map here
        return (
            <h1>MAP GOES HERE </h1>
        )
    }
}
