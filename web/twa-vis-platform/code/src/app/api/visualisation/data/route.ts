/**
 * Provides an API route that will return the data settings.
 * Used to communicate between client & server sides of the code.
 */
import { NextRequest } from 'next/server';
import DataSettingsStore from 'io/config/data-settings';

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET(request: NextRequest) {
    // Get URL query params
    const urlParams = request.nextUrl.searchParams;
    if(urlParams.get("invalidate") === "true") {
        DataSettingsStore.invalidate();
    }

    try {
        // Since getSettings is asynchronous, use await to get the result
        const result = await DataSettingsStore.getSettings();
        return new Response(JSON.stringify(result), {
            status: 200, // HTTP status code
            headers: {
                "Content-Type": "application/json",
            },
        });
    } catch (error) {
        // Handle possible errors, such as file not found or JSON parse errors
        return new Response(JSON.stringify({ error: "Failed to load data settings." }), {
            status: 500, // Internal Server Error
            headers: {
                "Content-Type": "application/json",
            },
        });
    }
}