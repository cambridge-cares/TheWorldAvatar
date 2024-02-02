/**
 * Server side code to read and cache data.json file.
 */

import fs from "fs";
import path from "path";
import { JsonObject } from "../../types/json";

/**
 * This class reads and stores data settings from the user provided "data.json" file.
 * Note that this only stores the raw JSON from the file, it is parsed into class 
 * instances on the client side.
 */
export default class DataSettingsStore {

    // Location of module settings file
    private static readonly DEFAULT_FILE = "../uploads/config/data.json";

    // Cached settings
    private static RAW_JSON: JsonObject;

    /**
    * Reads the settings file.
    * 
    * @param file optional config file path (relative to root).
    * 
    * @throws {IllegalArgumentError} if configuration file is invalid.
    */
    public static readFile(file?: string) {
        if(DataSettingsStore.RAW_JSON == null) {
            const configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
            const contents = JSON.parse(fs.readFileSync(configFile, "utf8"));
            DataSettingsStore.RAW_JSON = contents;

            console.info("Data settings have been read and cached.");
        }
    }

    /**
     * Returns the data settings.
     * 
     * @returns JSON settings object.
     */
    public static getSettings() {
        DataSettingsStore.readFile();
        return DataSettingsStore.RAW_JSON;
    }   

    /**
     * Invalidate the cached data settings, forcing them to be re-read
     * upon the next request.
     */
    public static invalidate() {
        console.log("Invalidating cached data settings, will be re-read upon next request.");
        DataSettingsStore.RAW_JSON = null;
    }

}
// End of class.