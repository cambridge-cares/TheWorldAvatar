/**
 * Server side code to read and cache all JSON configuration files.
 */

import fs from 'fs';
import path from 'path';
import { MapSettings } from 'types/settings';

/**
 * Handles the retrieval and storage of settings from the user provided configuration files.
 * Note that JSON is stored in its serialised form so that it can be passed from Server to Client component. 
 * Further parsing occurs on the client side.
 */
export default class SettingsStore {

  // Location of all configuration files
  private static readonly DEFAULT_SETTINGS_FILE: string = path.join(process.cwd(), "uploads/config/ui-settings.json");
  private static readonly DATA_SETTINGS_FILE: string = path.join(process.cwd(), "uploads/config/data.json");
  private static readonly MAP_SETTINGS_FILE: string = path.join(process.cwd(), "uploads/config/map-settings.json");
  private static readonly CSS_OVERRIDE_FILE: string = path.join(process.cwd(), "uploads/style-overrides.css");

  // Cached settings
  private static DEFAULT_SETTINGS: string | null = null;
  private static MAP_SETTINGS: string | null = null;

  /**
 * Returns true if the "style-overrides.css" file exists within
 * the hosted "uploads" directory.
 */
  public static hasCssOverrides(): boolean {
    return fs.existsSync(this.CSS_OVERRIDE_FILE);
  }

  /**
   * Retrieves default settings
   */
  public static getDefaultSettings(): string {
    if (!this.DEFAULT_SETTINGS) {
      this.readInitialisationSettings();
    }
    return this.DEFAULT_SETTINGS;
  }

  /**
  * Retrieves default settings
  */
  public static getMapSettings(): string {
    if (!this.MAP_SETTINGS) {
      this.readMapSettings();
    }
    return this.MAP_SETTINGS;
  }

  /**
   * Reads the initialisation settings.
   */
  public static readInitialisationSettings(): void {
    const settings: string = this.readFile(this.DEFAULT_SETTINGS_FILE);
    this.DEFAULT_SETTINGS = settings;
    console.info("Default settings have been read and cached.");
  }

  /**
   * Reads the map-related settings including the data and map.
   */
  public static readMapSettings(): void {
    const settings: string = this.readFile(this.MAP_SETTINGS_FILE);
    const dataSettings: string = this.readFile(this.DATA_SETTINGS_FILE);
    const mapSettings: MapSettings = JSON.parse(settings);
    mapSettings.data = JSON.parse(dataSettings);
    this.MAP_SETTINGS = JSON.stringify(mapSettings);
    console.info("Map settings have been read and cached.");
  }

  /**
   * Reads the Mapbox credentials from Docker secrets.
   * 
   * @returns Mapbox credentials.
   */
  public static getCredentials(): { username: string, key: string } {
    try {
      // Assuming these files exist in your Docker environment
      const username = this.readFile("/run/secrets/mapbox_user").trim();
      const key = this.readFile("/run/secrets/mapbox_api_key").trim();
      return { username, key };
    } catch (error) {
      // Fallback to environment variables
      const username = process.env.MAPBOX_USER;
      const key = process.env.MAPBOX_API_KEY;
      return { username: username || "unknown", key: key || "unknown" };
    }
  }

  /**
   * Read the input file
   * 
   * @param file Config file path.
   * @throws When the configuration file is invalid or not found.
   */
  private static readFile(file: string): string {
    const contents: string = fs.readFileSync(file, "utf-8");
    return contents;
  }
}
// End of class.