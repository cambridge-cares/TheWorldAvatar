import { AnyLayer, BackgroundLayer, CircleLayer, FillExtrusionLayer, FillLayer, HeatmapLayer, LineLayer, Map, RasterLayer, SymbolLayer } from 'mapbox-gl';

import { DataLayer } from 'io/data/data-layer';
import { DataStore } from 'io/data/data-store';
import { Interactions } from 'io/config/interactions';
import { JsonObject } from 'types/json';
import { ImageryOption, ImagerySettings } from 'types/settings';
import { getCurrentImageryOption } from 'map/map-helper';
/**
 * Given a DataStore instance housing parsed DataLayer instances,
 * this function adds them all to the Mapbox map instance.
 * 
 * @param {Map} map the Mapbox map.
 * @param {dataStore} dataStore Store containing parsed DataLayer instances.
 * @param {ImagerySettings} imagerySettings - The imagery settings for the map.
 */
export async function addAllLayers(map: Map, dataStore: DataStore, imagerySettings: ImagerySettings) {
    const currentStyle = getCurrentImageryOption(imagerySettings);

    const layerArray: DataLayer[] = dataStore?.getLayerList();
    layerArray?.forEach((layer) => addLayer(map, layer, currentStyle));
    console.log(`Added ${layerArray?.length} layers to the map object.`);
}

/**
 * Adds the input DataLayer to the Mapbox map instance.
 * 
 * @param {Map} map the Mapbox map instance.
 * @param {DataLayer} layer - The input DataLayer.
 * @param {ImageryOption} currentStyle - The current imagery style.
 */
export function addLayer(map: Map, layer: DataLayer, currentStyle: ImageryOption) {
    const collision = map?.getLayer(layer.id);

    if (collision != null) {
        console.warn("Attempting to add a layer that's already on map: '" + layer.id + "'.");
        return;
    }

    // Clone the original layer definition and adjust as needed
    const options: JsonObject = { ...layer.definition };
    options["id"] = layer.id;
    options["source"] = layer.source.id;
    // If there is a layout option, we must set visibility separately to prevent overriding the other suboptions
    if (options.layout) {
        const layoutOptions: JsonObject = options.layout as JsonObject;
        layoutOptions.visibility = layer.isGroupExpanded && layer.cachedVisibility ? "visible" : "none";
    } else {
        options.layout = {
            visibility: layer.isGroupExpanded && layer.cachedVisibility ? "visible" : "none"
        };
    }

    if (layer.getInjectableProperty(Interactions.HOVER)) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (options.paint as { [key: string]: any })["fill-opacity"] = layer.getInjectableProperty(Interactions.HOVER).style;
        delete options["hover"];
    }

    // Remove properties not expected by Mapbox
    delete options["interactions"];
    delete options["clickable"];
    delete options["treeable"];
    delete options["name"];
    delete options["order"];
    delete options["grouping"];

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
    map?.addLayer(mapboxObj);
    console.info("Pushed data layer to map '" + layer.id + "'.");
}