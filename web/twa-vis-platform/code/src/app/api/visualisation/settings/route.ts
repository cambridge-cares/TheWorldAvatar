/**
 * Provides an API route that will return the map configuration settings.
 * Used to communicate between client & server sides of the code.
 */
import fs from 'fs';

import MapSettingsStore from 'io/config/map-settings';
import { NextRequest, NextResponse } from 'next/server';

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET(request: NextRequest) {
    // Get URL query params
    const urlParams = request.nextUrl.searchParams;
    if(urlParams.get("invalidate") === "true") {
        MapSettingsStore.invalidate();
    }

    try {
        // Await the asynchronous call to getSettings
        const result = await MapSettingsStore.getSettings();

        // Read Mapbox credentials from Docker secrets or environment variables
        const credentials = getCredentials();
        result.credentials = credentials; // Ensure your MapSettings type can handle this additional property

        return new NextResponse(JSON.stringify(result), {
            status: 200,
            headers: {
                "Content-Type": "application/json"
            }
        });
    } catch (error) {
        console.error("Failed to retrieve map settings:", error);
        return new NextResponse(JSON.stringify({ error: "Failed to retrieve map settings" }), {
            status: 500,
            headers: {
                "Content-Type": "application/json"
            }
        });
    }
}

/**
 * Reads the Mapbox credentials from Docker secrets.
 * 
 * @returns Mapbox credentials.
 */
function getCredentials() {
    try {
        // Assuming these files exist in your Docker environment
        const username = fs.readFileSync("/run/secrets/mapbox_username", "utf-8").trim();
        const key = fs.readFileSync("/run/secrets/mapbox_api_key", "utf-8").trim();
        return { username, key };
    } catch(error) {
        // Fallback to environment variables
        const username = process.env.MAPBOX_USERNAME;
        const key = process.env.MAPBOX_API_KEY;
        return { username: username || "unknown", key: key || "unknown" };
    }
}