/* eslint-disable @typescript-eslint/no-explicit-any */

import fs from "fs";
import path from "path";

/**
 * Interface for map settings object.
 */
interface SettingsInterface {
    [key: string]: string | number | boolean | object;
}

/**
 * This class reads and stores non-data specific map settings from the user
 * provided "map-settings.json" file.
 */
export default class MapSettings {

    // Location of module settings file
    private static readonly DEFAULT_FILE = "../uploads/config/map-settings.json";

    // Cached settings
    private static SETTINGS: SettingsInterface;

    /**
    * Reads the settings file.
    * 
    * @param file optional config file path (relative to root).
    * 
    * @throws {IllegalArgumentError} if configuration file is invalid.
    */
    public static readSettings(file?: string) {
        if(MapSettings.SETTINGS == null) {
            const configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
            const contents = JSON.parse(fs.readFileSync(configFile, "utf8"));
            MapSettings.SETTINGS = contents;

            console.info("Map configuration settings have been read and cached.");
        }
    }

    /**
     * Returns the map configuration settings.
     * 
     * @param key optional key to return sub-object in settings.
     * @returns JSON settings object.
     */
    public static getSettings(key?: string) {
        MapSettings.readSettings();
        return (key != null) ? MapSettings.SETTINGS[key] : MapSettings.SETTINGS;
    }   

    /**
     * Invalidate the cached settings, forcing them to be re-read
     * upon the next request.
     */
    public static invalidate() {
        console.log("Invalidating cached map configuration settings, will be re-read upon next request.");
        MapSettings.SETTINGS = null;
    }

}
// End of class.