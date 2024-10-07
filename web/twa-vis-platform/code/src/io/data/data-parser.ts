import { JsonArray, JsonObject } from 'types/json';
import { ScenarioDimensionsData } from 'types/timeseries';
import { DataGroup } from './data-group';
import { DataLayer } from './data-layer';
import { LayerSource } from './layer-source';
import { DataStore } from './data-store';
import { MapboxDataLayer } from './mapbox/mapbox-data-layer';

/**
 * Handles parsing of raw JSON data into instances of the data classes.
 */
export class DataParser {
    private readonly mapType: string;
    // Data store to populate with parsed objects
    private readonly dataStore: DataStore;

    /**
     * Initialise a new DataParser instance.
     */
    constructor(mapType: string) {
        this.dataStore = new DataStore();
        this.mapType = mapType;
    }

    /**
     * Parse the input raw JSON into objects and store within the
     * current DataStore instance.
     * 
     * @param rawJson JSON of data.json file.
     */
    public loadData(rawJson: JsonObject): DataStore {
        if (Array.isArray(rawJson)) {
            rawJson.map((dataset) => {
                this.recurse(dataset, null, null, 0);
            })
        } else {
            this.recurse(rawJson, null, null, 0);
        }
        console.info("Data definition loading complete.");
        return this.dataStore;
    }

    /**
     * 
     * @param current current JSON node.
     * @param parentGroup parent DataGroup (if known).
     * @param stack current stack URL.
     * @param depth depth in JSON tree.
     */
    private recurse(current: JsonObject, parentGroup: DataGroup, stack: string, depth: number) {
        if (!current["name"]) {
            throw new Error("Cannot parse a DataGroup that has no name!")
        }

        // Retrieve the current stack for this group
        let currentStack: string;
        // If there is a stack property in the data.json, ensure that it is a string
        if (current["stack"]) {
            if (typeof current["stack"] === "string") {
                currentStack = current["stack"];
            } else {
                console.error("Unexpected type for 'stack' property");
                throw new Error("Unexpected type for 'stack' property")
            }
            // If there is no stack property, assume that it is inherited from the parent group. Else, leave as undefined
        } else {
            currentStack = (parentGroup != null) ? parentGroup.stackEndpoint : "undefined";
        }

        // Initialise data group
        const groupName: string = current["name"] as string;
        let isGroupExpanded: boolean = true;
        // Keep the check in this order
        if (Object.hasOwn(current, "expanded")) {
            isGroupExpanded = parentGroup?.isExpanded ? current["expanded"] as boolean : false;
        } else if (parentGroup) {
            isGroupExpanded = parentGroup.isExpanded;
        }
        const groupID: string = (parentGroup != null) ? (parentGroup.id + "." + depth) : depth.toString();
        const dataGroup: DataGroup = new DataGroup(groupName, groupID, currentStack, isGroupExpanded);

        // Store parent (if not root)
        if (parentGroup === null || parentGroup === undefined) {
            this.dataStore.addGroup(dataGroup);
        } else {
            dataGroup.parentGroup = parentGroup;
            parentGroup.subGroups.push(dataGroup);
        }

        // Parse sources and layers (if present)
        if (current["sources"]) {
            this.parseLayerSources(current["sources"] as JsonArray, dataGroup);
        }
        if (current["layers"]) {
            const currentLayerArray: JsonArray = current["layers"] as JsonArray;
            if (currentLayerArray.some(layer => Object.hasOwn(layer, "grouping"))) {
                this.verifyGroupings(currentLayerArray);
                dataGroup.layerGroupings = this.reorderGroupings(currentLayerArray);
            }
            this.parseDataLayers(current["layers"] as JsonArray, dataGroup);
        }

        // Add tree icon (if set)
        if (current["tree-icon"]) {
            dataGroup.treeIcon = current["tree-icon"] as string;
        }
        // Add search resource identifier (if set)
        if (current["search"]) {
            dataGroup.search = current["search"] as string;
        }
        // Recurse into sub groups (if present)
        if (current["groups"]) {
            const groupArray = current["groups"] as JsonArray;

            for (let i = 0; i < groupArray.length; i++) {
                const subNode = groupArray[i];
                this.recurse(subNode, dataGroup, stack, i);
            }
        }
    }

    /**
     * Parses the incoming JSON array into source objects and adds them
     * to the input data group.
     * 
     * @param sourceArray array of JSON source objects.
     * @param dataGroup group to add sources to.,
     */
    private parseLayerSources(sourceArray: JsonArray, dataGroup: DataGroup) {
        for (const element of sourceArray) {

            const sourceID = dataGroup.id + "." + (element["id"] as string);
            const source = new LayerSource(
                sourceID,
                element["type"] as string,
                dataGroup.stackEndpoint,
                element
            );

            dataGroup.layerSources.push(source);
        }
    }

    /**
    * Parses the incoming JSON array into layer objects and adds them
    * to the input data group.
    * 
    * @param layerArray array of JSON layer objects.
    * @param dataGroup group to add layer to.
    */
    private parseDataLayers(layerArray: JsonArray, dataGroup: DataGroup) {
        for (const element of layerArray) {
            const elementID = element["id"] as string;

            // Get matching source, ensure exists
            const originalSourceID = (element["source"] as string);
            const uniqueSourceID = this.recurseUpForSource(dataGroup, originalSourceID);

            const sourceObj = dataGroup.getFirstSourceWithID(uniqueSourceID);
            if (sourceObj == null) {
                console.error("Layer with ID '" + elementID + "' references a source that is not defined, will skip it!");
                continue;
            }

            // Create concrete class for data layer
            let layer: DataLayer;
            const layerID = dataGroup.id + "." + elementID;

            switch (this.mapType.toLowerCase()) {
                case "mapbox":
                    layer = new MapboxDataLayer(
                        layerID,
                        dataGroup.isExpanded,
                        sourceObj,
                        element,
                    );
                    // When there is a default grouping and this is not the current grouping, the layer should start off invisible
                    if (layer.grouping && String(layer.grouping) !== dataGroup.layerGroupings[0]) { layer.cachedVisibility = false; }
                    break;

                case "cesium":
                    throw new Error("Not yet implemented.");

                default:
                    throw new Error("Unknown map provider type, stopping execution.");
            }

            // Add order number (if set)
            if (element["order"] != null) {
                layer.order = element["order"] as number;
            }

            // Cache visibility & interaction level
            this.setVisibility(element, layer);
            this.setInteractions(element, layer);

            // Store the layer
            dataGroup.dataLayers.push(layer);
        }
    }

    /** Verify if the groupings are correctly set within the data.json.
     * 
     * @param {JsonArray} layerArray input array for verification.
     */
    private verifyGroupings(layerArray: JsonArray): void {
        // Ensure the grouping field is attached to all layers
        if (!layerArray.every(layer => Object.hasOwn(layer, "grouping"))) {
            throw new Error("Groupings detected for some layers. Please ensure all layers have a grouping field.");
        }
    }

    /** Reorder the layers so that default grouping always appear first.
     * 
     * @param {JsonArray} layerArray input array for reordering.
     */
    private reorderGroupings(layerArray: JsonArray): string[] {
        const uniqueGroupings: string[] = [...new Set(layerArray.map(layer => layer.grouping as string))];
        // Rearrange if 'Default' grouping exists
        const defaultIndex = uniqueGroupings.findIndex(grouping => grouping.toLowerCase().trim() === "default");
        if (defaultIndex !== -1) {
            // Remove the default string from the array
            const defaultString: string = uniqueGroupings.splice(defaultIndex, 1)[0];
            // Add the default string back at the beginning
            uniqueGroupings.unshift(defaultString);
        }
        return uniqueGroupings;
    }

    /**
     * 
     * @param element 
     * @param layer 
     */
    private setVisibility(element: JsonObject, layer: DataLayer) {
        // If the data group is visible, we then verify if the layer should be visible
        if (layer.isGroupExpanded) {
            const layoutObj = element["layout"] as JsonObject;
            if (layoutObj?.["visibility"] != null) {
                layer.cachedVisibility = (layoutObj["visibility"] == "visible");
            } else if (element["visibility"] != null) {
                // Support older format of this property
                layer.cachedVisibility = (element["visibility"] == "visible");
            }
        }
    }

    /**
     * 
     * @param element 
     * @param layer 
     */
    private setInteractions(element: JsonObject, layer: DataLayer) {
        if (element["interactions"] != null) {
            layer.interactions = element["interactions"] as string;
        } else if (element["clickable"] != null) {
            // Support older format of this property
            layer.interactions = (element["clickable"]) ? "all" : "none";
        }
    }

    /**
     * 
     * @param dataGroup 
     * @param sourceID original source ID (before prefix)
     * @returns 
     */
    private recurseUpForSource(dataGroup: DataGroup, sourceID: string): string {
        for (const source of dataGroup.layerSources) {
            const sourceDef = source.definition;
            if ((sourceDef["id"] as string) === sourceID) return source.id;
        }
        return this.recurseUpForSource(dataGroup.parentGroup, sourceID);
    }

    public static handleDimensions(element: JsonObject, scenarioDimensionsData: ScenarioDimensionsData, value: number): JsonObject {
        let stringified = JSON.stringify(element);
        for (const dimension of Object.keys(scenarioDimensionsData)) {
            stringified = stringified.replaceAll(`{` + dimension + `}`, value.toString())
        }
        return JSON.parse(stringified);
    }

}
// End of class.