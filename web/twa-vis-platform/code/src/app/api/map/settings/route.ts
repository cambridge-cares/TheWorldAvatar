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
export function GET() {
  try {
    const credentials = SettingsStore.getCredentials();
    return NextResponse.json({ token: credentials.key });
  } catch (error) {
    // Handle possible errors, such as file not found or JSON parse errors
    return NextResponse.json({ error: "Failed to retrieve credentials." + error }, { status: 500 });
  }
}