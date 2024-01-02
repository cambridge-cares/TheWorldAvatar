import MapSettings from "io/config/map-settings";
import { NextRequest } from "next/server";

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET(request: NextRequest) {
    console.log("Server has received request to read & return map configuration settings.");
    
    // Get URL query params
    const urlParams = request.nextUrl.searchParams;
    if(urlParams.get("invalidate") === "true") {
        MapSettings.invalidate();
    }

    // See if a sub-node key is present
    let result;

    if(urlParams.get("key") != null) {
        // Only return that sub-node within settings
        result = MapSettings.getSettings(urlParams.get("key"));
    } else {
        result = MapSettings.getSettings();
    }

    console.log("---");
    console.log(result);
    console.log("---");

    return Response.json(result);
}

