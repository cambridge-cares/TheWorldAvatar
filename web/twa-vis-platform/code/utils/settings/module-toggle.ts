import fs from "fs";
import path from "path";
import { ModuleToggleInterface, isValid } from "./interfaces/module-toggle-interface";

/**
 * This private class defines instances representing the possible configuration
 * options to toggle various UI Modules. Each instance defines the expected
 * configuration property key and the default value.
 */
export default class ModuleToggle {

    // Supported module toggles
    public static readonly TOOLBAR      = new ModuleToggle("toolbar", true);
    public static readonly LAYER_TREE   = new ModuleToggle("layer-tree", true);
    public static readonly LANDING      = new ModuleToggle("landing", true);

    // Location of module settings file
    private static readonly DEFAULT_FILE = "../config/ui-modules.json";

    // Cached module settings
    private static MODULE_SETTINGS: ModuleToggleInterface;

    /**
     * Private constructor.
     * 
     * @param key toggle property name. 
     * @param state default property state.
     */
    private constructor(public readonly key: string, public readonly state: boolean) {
        //Empty
    }

    /**
    * Reads the modules.json settings file.
    * 
    * @param file optional config file path (relative to root).
    * 
    * @throws {IllegalArgumentError} if module configuration file is invalid.
    */
    public static readModuleSettings(file?: string) {
        if(ModuleToggle.MODULE_SETTINGS == null) {
            let configFile = path.join(process.cwd(), file ?? this.DEFAULT_FILE);
            console.debug("Reading UI module settings file from: " + configFile);

            // Check config file validity
            let contents = JSON.parse(fs.readFileSync(configFile, "utf8"));
            isValid(contents);

            ModuleToggle.MODULE_SETTINGS = contents;
            console.debug("UI module settings have been read.");
        }
    }

    /**
     * Returns the UI module setting associated with the input key.
     * 
     * @param key setting parameter key.
     * 
     * @return resulting value.
     */
    public static getModuleStatus(module: ModuleToggle): boolean {
        if(ModuleToggle.MODULE_SETTINGS == null) {
            throw new Error("Cannot check module settings when configuration file has not been read.");
        }

        let key = module.key.toLowerCase();
        if(ModuleToggle.MODULE_SETTINGS?.hasOwnProperty(key)) {
            return ModuleToggle.MODULE_SETTINGS[key];
        } else {
            return module.state;
        }
    }

    /**
     * Returns the object containing all module toggles specified
     * in the loaded configuration file.
     * 
     * @returns loaded module toggles.
     */
    public static getAllModuleStatuses(): ModuleToggleInterface {
        return ModuleToggle.MODULE_SETTINGS;
    }

    /**
     * Clears the cached module settings.
     */
    public static clearModuleStatuses() {
        ModuleToggle.MODULE_SETTINGS = null;
    }
    
}
// End of class.