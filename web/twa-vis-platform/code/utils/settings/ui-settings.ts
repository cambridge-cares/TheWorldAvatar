import fs from "fs";
import path from "path";

/**
 * Interface for UI settings object.
 */
export declare interface SettingsInterface {
    branding: {
        [key: string]: string | number | boolean;
    },
    modules: {
        [key: string]: boolean
    }
}

/**
 * This class reads and stores miscellaneous UI settings from the user
 * provided "ui-settings.json" file.
 */
export default class UISettings {

    // Location of module settings file
    private static readonly DEFAULT_FILE = "../uploads/config/ui-settings.json";

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
        if(UISettings.SETTINGS == null) {
            let configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
            let contents = JSON.parse(fs.readFileSync(configFile, "utf8"));
            UISettings.SETTINGS = contents;

            console.info("UI settings have been read and cached.");
        }
    }

    /**
     * 
     * @returns 
     */
    public static getModuleSettings() {
        UISettings.readSettings();
        return UISettings.SETTINGS?.modules;
    }

    /**
     * 
     * @returns 
     */
     public static getBrandingSettings() {
        UISettings.readSettings();
        return UISettings.SETTINGS?.branding;
    }
}