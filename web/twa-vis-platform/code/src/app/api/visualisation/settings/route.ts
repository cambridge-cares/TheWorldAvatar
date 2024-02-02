/**
 * Provides an API route that will return the map configuration settings.
 * Used to communicate between client & server sides of the code.
 */
import fs from "fs";

import MapSettings from "io/config/map-settings";
import { NextRequest } from "next/server";

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET(request: NextRequest) {
    // Get URL query params
    const urlParams = request.nextUrl.searchParams;
    if(urlParams.get("invalidate") === "true") {
        MapSettings.invalidate();
    }

    // See if a sub-node key is present
    const result = MapSettings.getSettings();

    // Read Mapbox credentials from Docker secrets
    const credentials = getCredentials();
    result.credentials = credentials;

    return Response.json(result);
}

/**
 * Reads the Mapbox credentials from Docker secrets.
 * 
 * @returns Mapbox credentials.
 */
function getCredentials() {
    try {
        // Read from Docker secrets (should work in production).
        const username = fs.readFileSync("/run/secrets/mapbox_username", "utf-8");
        const key = fs.readFileSync("/run/secrets/mapbox_api_key", "utf-8");
        return { "username": username, "key": key };

    } catch(error) {
        // Attempt to read from environment variables (could be used in development).
        const username = process.env.MAPBOX_USERNAME;
        const key = process.env.MAPBOX_API_KEY;

        if(username == null || key == null) {
            return { "username": "unknown", "key": "unknown" };
        }
        return { "username": username, "key": key };
    }
}