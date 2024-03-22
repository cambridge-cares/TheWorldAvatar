import { AnyLayer, BackgroundLayer, CircleLayer, FillExtrusionLayer, FillLayer, HeatmapLayer, LineLayer, RasterLayer, SymbolLayer } from 'mapbox-gl';

import { DataLayer } from 'io/data/data-layer';
import { DataStore } from 'io/data/data-store';
import { JsonObject } from 'types/json';
import { ImageryOption } from 'types/map-settings';
import { getMapSettings } from 'utils/client-utils';
import { getCurrentImageryOption } from './mapbox-imagery-utils';
/**
 * Given a DataStore instance housing parsed DataLayer instances,
 * this function adds them all to the Mapbox map object.
 * 
 * @param {dataStore} dataStore Store containing parsed DataLayer instances.
 * @param {Dispatch<any>} dispatch - The dispatch function from Redux for dispatching actions.
 */
export async function addAllLayers(dataStore: DataStore) {
    const mapSettings = await getMapSettings();
    const currentStyle = getCurrentImageryOption(mapSettings);

    const layerArray: DataLayer[] = dataStore.getLayerList();
    layerArray.forEach((layer) => addLayer(layer, currentStyle));
    console.log("Added all registered layers to the map object.");
}

/**
 * Adds the input DataLayer to the Mapbox map object.
 * 
 * @param {DataLayer} layer - The input DataLayer.
 * @param {ImageryOption} currentStyle - The current imagery style.
 */
export function addLayer(layer: DataLayer, currentStyle: ImageryOption) {
    const collision = window.map.getLayer(layer.id);

    if (collision != null) {
        console.warn("Attempting to add a layer that's already on map: '" + layer.id + "'.");
        return;
    }

    // Clone the original layer definition and adjust as needed
    const options: JsonObject = { ...layer.definition };
    options["id"] = layer.id;
    options["source"] = layer.source.id;
    if (options.layout) {
        const layoutOptions: JsonObject = options.layout as JsonObject;
        layoutOptions.visibility = layer.cachedVisibility ? "visible" : "none";
    } else {
        options.layout = {
            visibility: "visible"
        };
    }
    // Remove properties not expected by Mapbox
    delete options["interactions"];
    delete options["clickable"];
    delete options["treeable"];
    delete options["name"];
    delete options["order"];

    // Add attributions if missing
    if (!options["metadata"]) {
        options["metadata"] = {
            attribution: "CMCL"
        }
    }

    // Add slot setting if using v3 style
    const isStandard = (currentStyle.url.includes("standard") || currentStyle.time != null);
    if (isStandard && options["slot"] == null) {
        options["slot"] = "top";
    }
    // Have to cast to type specific object to meet Mapbox's API
    let mapboxObj: AnyLayer;
    const layerType = layer.definition["type"];

    let paintObj = options["paint"] as JsonObject;
    if (paintObj == null) {
        paintObj = {};
        options["paint"] = paintObj;
    }

    switch (layerType as string) {
        case "background":
            if (isStandard) paintObj["background-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as BackgroundLayer);
            break;
        case "circle":
            if (isStandard) paintObj["circle-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as CircleLayer);
            break;
        case "fill-extrusion":
            if (isStandard) paintObj["fill-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as FillExtrusionLayer);
            break;
        case "fill":
            if (isStandard) paintObj["fill-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as FillLayer);
            break;
        case "heatmap":
            mapboxObj = ((options as unknown) as HeatmapLayer);
            break;
        case "line":
            if (isStandard) paintObj["line-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as LineLayer);
            break;
        case "raster":
            mapboxObj = ((options as unknown) as RasterLayer);
            break;
        case "symbol":
            if (isStandard) paintObj["icon-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as SymbolLayer);
            break;
    }

    // Add to the map
    window.map.addLayer(mapboxObj);
    console.info("Pushed data layer to map '" + layer.id + "'.");
}