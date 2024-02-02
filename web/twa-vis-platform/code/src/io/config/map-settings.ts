/**
 * Server side code to read and cache map-settings.json file.
 */

import fs from "fs";
import path from "path";

import { MapSettings } from "types/map-settings";

/**
 * This class reads and stores non-data specific map settings from the user
 * provided "map-settings.json" file.
 */
export default class MapSettingsStore {

    // Location of module settings file
    private static readonly DEFAULT_FILE = "../uploads/config/map-settings.json";

    // Cached settings
    private static SETTINGS: MapSettings;

    /**
    * Reads the settings file.
    * 
    * @param file optional config file path (relative to root).
    * 
    * @throws {IllegalArgumentError} if configuration file is invalid.
    */
    public static readSettings(file?: string) {
        if(MapSettingsStore.SETTINGS == null) {
            const configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
            const contents = JSON.parse(fs.readFileSync(configFile, "utf8"));
            MapSettingsStore.SETTINGS = contents;

            console.info("Map configuration settings have been read and cached.");
        }
    }

    /**
     * Returns the map configuration settings.
     * 
     * @returns JSON settings object.
     */
    public static getSettings() {
        MapSettingsStore.readSettings();
        return MapSettingsStore.SETTINGS;
    }   

    /**
     * Invalidate the cached settings, forcing them to be re-read
     * upon the next request.
     */
    public static invalidate() {
        console.log("Invalidating cached map configuration settings, will be re-read upon next request.");
        MapSettingsStore.SETTINGS = null;
    }

}
// End of class.