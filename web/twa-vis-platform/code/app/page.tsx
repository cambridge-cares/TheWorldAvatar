import { Metadata } from "next";

import LandingPage from "../components/landing-page";
import UISettings from "@/utils/settings/ui-settings";
import { OptionalPages } from "@/utils/settings/optional-pages";

/**
 * Set page metadata.
 * 
 * @returns metadata promise.
 */
export async function generateMetadata(): Promise<Metadata> {
    if(UISettings.getModuleSettings().landing) {
        let landingPage = OptionalPages.getPage("landing");
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
export default function Home() {
    if(UISettings.getModuleSettings().landing) {
        // Enabled, load components for that here
        return LandingPage();
        
    } else {
        // Disabled, load straight into map here
        return (
            <h1>MAP GOES HERE </h1>
        )
    }
}
