/**
 * Server side code to read and cache all JSON configuration files.
 */

import fs from 'fs';
import path from 'path';

import { JsonObject } from 'types/json';

/**
 * Handles the retrieval and storage of settings from the user provided configuration files.
 * Note that JSON is stored in its serialised form so that it can be passed from Server to Client component. 
 * Further parsing occurs on the client side.
 */
export default class SettingsStore {

  // Location of all configuration files
  private static readonly DEFAULT_SETTINGS_FILE: string = path.join(process.cwd(), "public/config/ui-settings.json");
  private static readonly DATA_SETTINGS_FILE: string = path.join(process.cwd(), "public/config/data-settings.json");
  private static readonly MAP_SETTINGS_FILE: string = path.join(process.cwd(), "public/config/map-settings.json");
  private static readonly CSS_OVERRIDE_FILE: string = path.join(process.cwd(), "public/style-overrides.css");

  // Cached settings
  private static DEFAULT_SETTINGS: string | null = null;
  private static MAP_SETTINGS: string | null = null;
  private static MAP_DATA_SETTINGS: string | null = null;

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
   * Retrieves default map settings
   */
  public static getMapSettings(): string {
    if (!this.MAP_SETTINGS) {
      this.readMapSettings();
    }
    return this.MAP_SETTINGS;
  }

  /**
   * Retrieves data settings for populating map.
   */
  public static getMapDataSettings(): string {
    if (!this.MAP_DATA_SETTINGS) {
      this.readMapDataSettings();
    }
    return this.MAP_DATA_SETTINGS;
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
   * Reads the map settings.
   */
  public static readMapSettings(): void {
    const settings: string = this.readFile(this.MAP_SETTINGS_FILE);
    this.MAP_SETTINGS = settings;
    console.info("Map settings have been read and cached.");
  }

  /**
 * Reads the data settings for populating the map.
 */
  public static async readMapDataSettings(): Promise<void> {
    try {
      // Retrieve datasets from data settings file
      const dataSettings: string = this.readFile(this.DATA_SETTINGS_FILE);
      const datasets: string[] = JSON.parse(dataSettings).dataSets;

      // Array of promises to fetch data from each dataset
      const dataPromises: Promise<JsonObject>[] = (datasets.map(async dataset => {
        let jsonData: JsonObject;
        // Local datasets will start with /, and must have public appended
        if (dataset.startsWith("/")) {
          jsonData = JSON.parse(this.readFile("public" + dataset));
        } else {
          // For remote datasets, fetch the json
          const res = await fetch(dataset);
          if (res.ok) {
            jsonData = await res.json();
          }
        }
        return jsonData;
      }));

      // Wait for all promises to resolve, filter out null values, and stringify the resulting array
      const data: JsonObject[] = (await Promise.all(dataPromises)).filter(Boolean);
      this.MAP_DATA_SETTINGS = JSON.stringify(data);
      console.info("Map data settings have been read and cached.");
    } catch (error) {
      console.info("No local data files detected...");
    }
  }

  /**
   * Reads the Mapbox credentials from Docker secrets.
   * 
   * @returns Mapbox credentials.
   */
  public static getCredentials(): { username: string, key: string } {
    try {
      // Assuming these files exist in your Docker environment
      const username = this.readFile("/run/secrets/mapbox_username").trim();
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