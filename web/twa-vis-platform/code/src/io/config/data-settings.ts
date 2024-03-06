/**
 * Server side code to read and cache data.json file.
 */

import fs from 'fs/promises'; // Use the promise-based version of fs
import path from 'path';
import { JsonObject } from 'types/json';

/**
 * This class reads and stores data settings from the user provided "data.json" file.
 * Note that this only stores the raw JSON from the file, it is parsed into class 
 * instances on the client side.
 */
export default class DataSettingsStore {

    // Location of module settings file
    private static readonly DEFAULT_FILE = process.env.DATA_SETTINGS_PATH || "../uploads/config/data.json";

    // Cached settings
    private static RAW_JSON: JsonObject | null = null;

    /**
     * Asynchronously reads the settings file.
     * 
     * @param file Optional config file path (relative to root).
     * @throws When the configuration file is invalid or not found.
     */
    public static async readFile(file?: string): Promise<void> {
        if (DataSettingsStore.RAW_JSON === null) {
            try {
                const configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
                const contents = await fs.readFile(configFile, "utf8");
                DataSettingsStore.RAW_JSON = JSON.parse(contents);
                console.info("Data settings have been read and cached.");
            } catch (error) {
                console.error("Failed to read or parse the data settings file.", error);
                throw new Error("Failed to read or parse the data settings file.");
            }
        }
    }

    /**
     * Returns the data settings, ensuring they are read and cached first.
     * 
     * @returns JSON settings object.
     */
    public static async getSettings(): Promise<JsonObject> {
        await DataSettingsStore.readFile();
        return DataSettingsStore.RAW_JSON!;
    }

    /**
     * Invalidate the cached data settings, forcing them to be re-read upon the next request.
     */
    public static invalidate(): void {
        console.log("Invalidating cached data settings. They will be re-read upon next request.");
        DataSettingsStore.RAW_JSON = null;
    }

}
// End of class.