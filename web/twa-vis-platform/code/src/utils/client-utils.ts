/**
 * Utilities to be run on the client.
 */
import moment from 'moment';

import { DataParser } from 'io/data/data-parser';
import { DataStore } from 'io/data/data-store';
import { JsonObject } from "types/json";
import { TimeSeriesGroup, TimeSeries } from 'types/timeseries';

/**
 * Open full screen mode.
 */
export function openFullscreen() {
    const elem = document?.documentElement;
    if (elem?.requestFullscreen) {
        elem.requestFullscreen();
    }
}

/**
 * Close fullscreen mode.
 */
export function closeFullscreen() {
    if (document?.exitFullscreen) {
        document.exitFullscreen();
    }
}

/**
 * Parses the contents of the data.json file into class instances for setting the map.
 * 
 * @param dataSettings Map data settings.
 * @param mapType The type of map. Either Cesium or Mapbox
 * @returns The data model required for visualisation.
 */
export function parseMapDataSettings(dataSettings: JsonObject, mapType: string): DataStore {
    return new DataParser(mapType).loadData(dataSettings);
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