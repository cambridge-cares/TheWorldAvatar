import { DataStore } from 'io/data/data-store';
import { LayerSource } from 'io/data/layer-source';
// @ts-expect-error -- The MapBox types are inconsistent and all over the place but this works
import { AnySourceData, CanvasSourceRaw, GeoJSONSourceRaw, ImageSourceRaw, Map, RasterSource, VectorSource, VideoSourceRaw } from 'mapbox-gl';
import { JsonObject } from 'types/json';

/**
 * Given a DataStore instance housing parsed LayerSource instances,
 * this function adds them all to the Mapbox map object.
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {LayerSource} dataStore Store containing parsed LayerSource instances.
 */
export function addAllSources(map: Map, dataStore: DataStore) {
    const sourceArray: LayerSource[] = dataStore?.getSourceList();
    try {
        sourceArray?.forEach((source) => addSource(map, source));
        console.info(`Added ${sourceArray?.length} registered sources to the map object.`);
    } catch (error) {
        console.error("Couldn't add layer sources to map", error)
    }
}

/**
 * Adds the input data source to the Mapbox map object.
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {LayerSource} source data source to add.
 */
export function addSource(map: Map, source: LayerSource) {
    const collision = map?.getSource(source.id);
    if (collision) {
        console.warn(`Attempting to add a source that's already on map: '${source.id}'.`);
        return;
    }

    // Clone the original source definition
    const options: JsonObject = { ...source.definition };
    // Remove properties not expected by Mapbox
    delete options["id"];
    delete options["metaFiles"];
    delete options["timeseriesFiles"];

    // Add attributions if missing
    options["attribution"] = options["attribution"] ?? "CMCL";
    options["attribution"] = `Powered by <a style="text-color:rgba(0,0,0,.5)" href="https://theworldavatar.io/" target = "_blank" title = "TWA" aria - label="TheWorldAvatar">The World Avatar&#8482;</a> | <a href="https://cmcl.io/" target = "_blank" title = "CMCL" aria - label="CMCL">${options["attribution"]} </a>`;

    // Have to cast to type specific object to meet Mapbox's API

    
    let mapboxObj: AnySourceData;
    switch (source.type) {
        case "canvas":
            mapboxObj = ((options as unknown) as CanvasSourceRaw);
            break;
        case "geojson":
            mapboxObj = ((options as unknown) as GeoJSONSourceRaw);
            break;
        case "image":
            mapboxObj = ((options as unknown) as ImageSourceRaw);
            break;
        case "raster":
            mapboxObj = ((options as unknown) as RasterSource);
            break;
        case "vector":
            mapboxObj = ((options as unknown) as VectorSource);
            break;
        case "video":
            mapboxObj = ((options as unknown) as VideoSourceRaw);
            break;
        default:
            throw new Error(`Unsupported source type: ${source.type}`);
    }

    // Add to the map
    map?.addSource(source.id, mapboxObj);
    console.info(`Pushed data source: ${source.id} to map`);
}