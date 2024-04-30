/**
 * Provides an API route that will return the data settings.
 * Used to communicate between client & server sides of the code.
 */
import { NextResponse } from 'next/server'
import SettingsStore from 'io/config/settings';

// Prevents caching
export const dynamic = 'force-dynamic';

/**
 * Respond to incoming HTTP requests with map settings.
 */
export async function GET() {
  try {
    // Since getSettings is asynchronous, use await to get the result
    const credentials = await SettingsStore.getCredentials();
    return NextResponse.json({ token: credentials.key });
  } catch (error) {
    // Handle possible errors, such as file not found or JSON parse errors
    return NextResponse.json({ error: "Failed to retrieve credentials." }, { status: 500 });
  }
}