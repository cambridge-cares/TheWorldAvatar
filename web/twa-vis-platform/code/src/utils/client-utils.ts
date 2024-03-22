/**
 * Utilities to be run on the client.
 */
import moment from 'moment';

import { DataStoreCache } from 'io/data/data-store-cache';
import { DataParser } from 'io/data/data-parser';
import { DataStore } from 'io/data/data-store';
import { JsonObject } from "types/json";
import { MapSettings } from 'types/map-settings';
import { TimeSeriesGroup, TimeSeries } from 'types/timeseries';

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

/**
 * Parse the time series data returned from agents into the time series group data model.
 *
 * @param {JsonObject} data The JSON data to parse.
 * @returns {TimeSeriesGroup} The required data model.
 */
export function parseTimeSeries(data: JsonObject): TimeSeriesGroup {
    // Initialise new empty array to store the values
    const timeSeries: TimeSeries[] = [];
    const times: moment.Moment[] = [];

    // Parse data response to comply with typescript
    const timeData: JsonObject = JSON.parse(JSON.stringify(data.time))[0];
    const tsNames: Array<string> = JSON.parse(JSON.stringify(timeData.data));
    const tsUnits: Array<string> = JSON.parse(JSON.stringify(timeData.units));
    const tsValues: Array<Array<number>> = JSON.parse(JSON.stringify(timeData.values));
    const tsValueClass: Array<string> = JSON.parse(JSON.stringify(timeData.valuesClass));
    const timeClass: string = timeData.timeClass as string;
    const rawTimes: number[] = JSON.parse(JSON.stringify(timeData.time));

    for (let t = 0; t < rawTimes.length; t++) {
        if (timeClass === "dateTime" || timeClass === "Instant") {
            times.push(moment(rawTimes[t], "YYYY-MM-DD HH:mm:ss"));
        } else if (timeClass === "offsetTime") {
            times.push(moment(rawTimes[t], "HH:mm:ss"));
        }
    }

    // Extract the current values and unit for each time series
    tsNames.map((name, index) => {
        timeSeries.push({
            name: name,
            unit: tsUnits[index],
            values: tsValues[index],
            valuesClass: tsValueClass[index],
        });
    })
    return {
        id: timeData.id as number,
        timeClass: timeData.timeClass as string,
        momentTimes: times,
        times: rawTimes,
        data: timeSeries,
    };
}