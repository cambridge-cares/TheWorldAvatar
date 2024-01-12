import { AnySourceData, CanvasSourceRaw, GeoJSONSourceRaw, ImageSourceRaw, RasterSource, VectorSource, VideoSourceRaw } from "mapbox-gl";
import { DataSource } from "../../io/data/data-source";
import { JsonObject } from "../../types/json";

/**
 * Adds the input data source to the Mapbox map object.
 * 
 * @param source data source to add.
 */
export function addSource(source: DataSource) {
    const collision = window.map.getSource(source.id);
    if(collision != null) {
        console.warn("Attempting to add source that's already on map: '" + source.id + "'.");
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
    window.map.addSource(source.id, mapboxObj);
    console.info("Pushed data source to map '" + source.id + "'.");
}