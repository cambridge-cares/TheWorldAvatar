/**
 * Utilities to be run on the client.
 */
import { Dispatch } from 'redux';

import { DataParser } from 'io/data/data-parser';
import { DataStore } from 'io/data/data-store';
import { MapFeaturePayload, clearFeatures, setIri, setProperties, setStack } from 'state/map-feature-slice';
import { JsonObject } from "types/json";

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
 * Set the selected feature and its required properties in Redux state for global access.
 *
 * @param {MapFeaturePayload} selectedFeature The feature of interest.
 * @param {Dispatch<any>} dispatch The dispatch function from Redux for dispatching actions.
 */
export function setSelectedFeature(selectedFeature: MapFeaturePayload, dispatch: Dispatch): void {
    if (selectedFeature) {
        // Disable linting as we wish to remove layer but do not require it in this function
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const { _layer, stack, iri, ...selectedProperties } = selectedFeature;
        if (!iri) {
            console.warn("IRI is missing. Data fetching will be skipped.");
        } else if (!stack) {
            console.warn("Feature does not have a defined stack. Data fetching will be skipped.");
        }
        dispatch(setIri(iri));
        dispatch(setProperties(selectedProperties));
        dispatch(setStack(stack));
        dispatch(clearFeatures());
    }
}

/**
 * Capitalises the words.
 * 
 * @param {string} str input string.
 */
export function parseWordsForLabels(str: string): string {
    if (isValidIRI(str)) {
        return getAfterDelimiter(str, "/");
    }
    return str.toLowerCase()
        .replaceAll("_", " ")
        .replace(/([a-z])([A-Z])/g, '$1 $2')
        .split(" ")
        .map(word => word.charAt(0).toUpperCase() + word.slice(1))
        .join(" ");
}

/**
 * Checks that the input iri is valid.
 * 
 * @param {string} iri input iri.
 */
export function isValidIRI(iri: string): boolean {
    // eslint-disable-next-line
    const iriPattern = /^(https?|ftp|mailto|file|data|irc|tel|urn|uuid|doi):((\/\/[^\/?#]*)?[^?#]*)(\?[^#]*)?(#.*)?$/i;
    return iriPattern.test(iri);
}

/**
 * Retrieves the string following the delimiter if it exists. Otherwise returns the string as is.
 * 
 * @param {string} str input string.
 * @param {string} delimiter delimiter of interest.
 */
export function getAfterDelimiter(str: string, delimiter: string): string {
    return str.includes(delimiter) ? str.split(delimiter).pop() : str;
}