/**
 * Server side code to read and cache map-settings.json file.
 */

import fs from 'fs/promises';
import path from "path";
import { MapSettings } from "types/map-settings";

/**
 * This class reads and stores non-data specific map settings from the user
 * provided "map-settings.json" file.
 */
export default class MapSettingsStore {

    // Define the default file path for the map settings JSON file.
    private static readonly DEFAULT_FILE = "../uploads/config/map-settings.json";

    // Cached settings
    private static SETTINGS: MapSettings | null = null;

    /**
    * Asynchronously reads the map settings file and caches it.
    * 
    * @param file An optional parameter to specify a different file path for the map settings.
    * 
    * @throws {IllegalArgumentError} if configuration file is invalid.
    */
    public static async readSettings(file?: string): Promise<void> {
        if (!MapSettingsStore.SETTINGS) {
            try {
                // Construct the full path to the map settings file.
                const configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
                // Read the file content asynchronously.
                const contents = await fs.readFile(configFile, "utf8");
                // Parse the JSON content and cache it.
                MapSettingsStore.SETTINGS = JSON.parse(contents) as MapSettings;
                console.info("Map configuration settings have been read and cached.");
            } catch (error) {
                // Log and rethrow any errors encountered during file reading or parsing.
                console.error("Failed to read or parse the map settings file.", error);
                throw new Error("Failed to read or parse the map settings file.");
            }
        }
    }

    /**
     * Returns the cached map settings.
     * If the settings have not been read yet, it initiates reading and caching them first.
     * @returns The cached map settings object.
     */
    public static async getSettings(): Promise<MapSettings> {
        if (!MapSettingsStore.SETTINGS) {
            await MapSettingsStore.readSettings();
        }
        return MapSettingsStore.SETTINGS!;
    } 

    /**
     * Invalidates the cached settings, forcing them to be re-read upon the next request.
     */
    public static invalidate(): void {
        console.log("Invalidating cached map configuration settings. They will be re-read upon next request.");
        MapSettingsStore.SETTINGS = null;
    }

}
// End of class.