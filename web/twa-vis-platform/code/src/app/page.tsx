import React from "react";
import { Metadata } from "next";

import LandingPage from "ui/pages/landing";
import UISettings from "io/config/ui-settings";

import MapContainer from "./visualisation/page";
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
    
    return (
        <>
            {moduleSettings.landing &&
                <LandingPage/>
            }
            {!moduleSettings.landing &&
                <MapContainer/>
            }
        </>
    );
}
