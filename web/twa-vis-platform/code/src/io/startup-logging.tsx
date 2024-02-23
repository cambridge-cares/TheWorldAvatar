/**
 * Provides some logging at startup.
 */

"use client";

import React from "react";

// Prevent logging multiple times
let done: boolean = false;

/**
 * Empty component that runs on the client, allowing access
 * to basic in-browser logging functions.
 * @returns 
 */
export default function StartupLogging() {
    if(!done) logTheme();
    done = true;

    // Return empty component.
    return (<></>);
}

/**
 * Logs some debugging information when app is accessed.
 */
function logTheme() {
    if(typeof window === "undefined") return;
    
    const darkTheme = window.matchMedia("(prefers-color-scheme: dark)");
    if(darkTheme.matches) {
        console.info("Accessed with preference for DARK theme.");
    } else {
        console.info("Accessed with preference for LIGHT theme (or no preference).");
    }
}