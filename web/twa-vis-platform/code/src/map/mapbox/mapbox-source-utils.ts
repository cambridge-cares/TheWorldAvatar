import { AnySourceData, CanvasSourceRaw, GeoJSONSourceRaw, ImageSourceRaw, Map, RasterSource, VectorSource, VideoSourceRaw } from 'mapbox-gl';
import { DataSource } from 'io/data/data-source';
import { DataStore } from 'io/data/data-store';
import { JsonObject } from 'types/json';

/**
 * Given a DataStore instance housing parsed DataSource instances,
 * this function adds them all to the Mapbox map object.
 * 
 * @param {React.MutableRefObject<Map>} map the Mapbox map instance wrapped in a mutable reference object.
 * @param {DataSource} dataStore Store containing parsed DataSource instances.
 */
export function addAllSources(map: React.MutableRefObject<Map>, dataStore: DataStore) {
    const sourceArray: DataSource[] = dataStore.getSourceList();
    sourceArray.forEach((source) => addSource(map, source));
    console.log("Added all registered sources to the map object.");
}

/**
 * Adds the input data source to the Mapbox map object.
 * 
 * @param {React.MutableRefObject<Map>} map the Mapbox map instance wrapped in a mutable reference object.
 * @param {DataSource} source data source to add.
 */
export function addSource(map: React.MutableRefObject<Map>, source: DataSource) {
    const collision = map.current?.getSource(source.id);
    if(collision != null) {
        console.warn("Attempting to add a source that's already on map: '" + source.id + "'.");
        return;
    }

    // Clone the original source definition
    const options: JsonObject = {...source.definition};

    // Remove properties not expected by Mapbox
    if(options["id"] != null) delete options["id"];
    if(options["metaFiles"] != null) delete options["metaFiles"];
    if(options["timeseriesFiles"] != null) delete options["timeseriesFiles"];

    // Add attributions if missing
    if(!options["attribution"]) {
        options["attribution"] = "CMCL";
    }

    // Have to cast to type specific object to meet Mapbox's API
    let mapboxObj: AnySourceData;
    switch(source.type) {
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
    }

    // Add to the map
    map.current?.addSource(source.id, mapboxObj);
    console.info("Pushed data source to map '" + source.id + "'.");
}