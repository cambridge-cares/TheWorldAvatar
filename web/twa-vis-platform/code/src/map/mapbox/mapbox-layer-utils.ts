import { AnyLayer, BackgroundLayer, CircleLayer, FillExtrusionLayer, FillLayer, HeatmapLayer, LineLayer, RasterLayer, SymbolLayer } from "mapbox-gl";
import { DataLayer } from "../../io/data/data-layer";
import { DataStore } from "../../io/data/data-store";
import { JsonArray, JsonObject } from "../../types/json";
import { getCurrentImageryOption } from "./mapbox-imagery-utils";
import { ImageryOption, MapSettings } from "../../types/map-settings";
import { getMapSettings } from "../../utils/client-utils";

/**
 * Given a DataStore instance housing parsed DataLayer instances,
 * this function adds them all to the Mapbox map object.
 * 
 * @param dataStore Store containing parsed DataLayer instances.
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
 * @param source data source to add.
 */
export function addLayer(layer: DataLayer, currentStyle: ImageryOption) {
    const collision = window.map.getLayer(layer.id);

    if(collision != null) {
        console.warn("Attempting to add a layer that's already on map: '" + layer.id + "'.");
        return;
    }

    // Clone the original layer definition and adjust as needed
    const options: JsonObject = {...layer.definition};
    options["id"] = layer.id;
    options["source"] = layer.source.id;

    // Remove properties not expected by Mapbox
    delete options["interactions"];
    delete options["clickable"];
    delete options["treeable"];
    delete options["name"];
    delete options["order"];

    // Add attributions if missing
    if(!options["metadata"]) {
        options["metadata"] = {
            attribution: "CMCL"
        }
    }

    // Add slot setting if using v3 style
    const isStandard = (currentStyle.url.includes("standard") || currentStyle.time != null);
    if(isStandard && options["slot"] == null) {
        options["slot"] = "top";
    }
    // Have to cast to type specific object to meet Mapbox's API
    let mapboxObj: AnyLayer;
    const layerType = layer.definition["type"];

    let paintObj = options["paint"] as JsonObject;
    if(paintObj == null) {
        paintObj = {};
        options["paint"] = paintObj;
    }

    switch(layerType as string) {
        case "background":
            if(isStandard) paintObj["background-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as BackgroundLayer);
        break;
        case "circle":
            if(isStandard) paintObj["circle-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as CircleLayer);
        break;
        case "fill-extrusion":
            if(isStandard) paintObj["fill-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as FillExtrusionLayer);
        break;
        case "fill":
            if(isStandard) paintObj["fill-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as FillLayer);
        break;
        case "heatmap":
            mapboxObj = ((options as unknown) as HeatmapLayer);
        break;
        case "line":
            if(isStandard) paintObj["line-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as LineLayer);
        break;
        case "raster":
            mapboxObj = ((options as unknown) as RasterLayer);
        break;
        case "symbol":
            if(isStandard) paintObj["icon-emissive-strength"] = 1.0;
            mapboxObj = ((options as unknown) as SymbolLayer);
        break;
    }

    // Add to the map
    window.map.addLayer(mapboxObj);
    console.info("Pushed data layer to map '" + layer.id + "'.");

    // Attach a click event listener specific to this layer
    window.map.on('click', layer.id, (e) => {
        // Accessing the first feature in the array of features under the click point
        const feature = e.features && e.features[0];

        if (feature) {
            // Here you can access the metadata of the clicked feature
            console.log(`Clicked on ${layer.id}:`, feature.properties);

            // Perform additional actions with the feature's properties (metadata) here
            // For example, displaying this information in a UI component
        }
    });

}