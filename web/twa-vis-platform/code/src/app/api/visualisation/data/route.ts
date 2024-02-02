/**
 * Provides an API route that will return the data settings.
 * Used to communicate between client & server sides of the code.
 */
import { NextRequest } from "next/server";
import DataSettingsStore from "io/config/data-settings";

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET(request: NextRequest) {
    // Get URL query params
    const urlParams = request.nextUrl.searchParams;
    if(urlParams.get("invalidate") === "true") {
        DataSettingsStore.invalidate();
    }

    // See if a sub-node key is present
    const result = DataSettingsStore.getSettings();
    return Response.json(result);
}
