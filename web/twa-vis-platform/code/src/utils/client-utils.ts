/**
 * Utilities to be run on the client.
 */

import { json } from "express";
import { DataStoreCache } from "../io/data/data-store-cache";
import { DataParser } from "../io/data/data-parser";
import { DataStore } from "../io/data/data-store";
import { MapSettings } from "../types/map-settings";

/**
 * Open full screen mode.
 */
export function openFullscreen() {
    const elem = document?.documentElement;
    if(elem?.requestFullscreen) {
        elem.requestFullscreen();
    } 
}
  
/**
 * Close fullscreen mode.
 */
export function closeFullscreen() {
    if(document?.exitFullscreen) {
        document.exitFullscreen();
    }
}

/**
 * Query the server to get the map-settings.json file.
 */
export async function getMapSettings(): Promise<MapSettings> {
    return fetch("/api/visualisation/settings", { cache: "force-cache" })
            .then((result) => result.json())
            .catch((err) => console.log(err));
}

/**
 * Query the server to get the data.json file.
 * 
 * @param jsonFileURL optional override for location of data file.
 */
export async function getDataSettings(jsonFileURL?: string) {
    const url = jsonFileURL ?? "/api/visualisation/data";
    return fetch(url, { cache: "force-cache" })
            .then((result) => result.json())
            .catch((err) => console.log(err));
}

/**
 * Fetch the contents of the data.json file AND parses it into class instances
 * to be cached in the DataStoreCache singleton.
 * 
 * @param jsonFileURL optional override for location of data file.
 * 
 * @returns promise that resolves after fetch and parse.
 */
export async function getAndParseDataSettings(jsonFileURL?: string): Promise<DataStore> {
    const key = jsonFileURL ?? "local";

    // Return cached version if present
    if(DataStoreCache.STORES[key] != null) {
        return Promise.resolve(DataStoreCache.STORES[key]);
    }

    // Fetch and parse from data.json
    const fetchPromise = getDataSettings(jsonFileURL);
    
    return fetchPromise.then((json) => {
        const dataStore: DataStore = new DataParser().loadData(json);
        DataStoreCache.STORES[key] = dataStore;
        return dataStore;
    });
}