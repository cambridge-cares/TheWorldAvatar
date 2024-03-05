import { JsonArray, JsonObject } from "../../types/json";
import { DataGroup } from "./data-group";
import { DataLayer } from "./data-layer";
import { DataSource } from "./data-source";
import { DataStore } from "./data-store";
import { MapboxDataLayer } from "./mapbox/mapbox-data-layer";

/**
 * Handles parsing of raw JSON data into instances of the data classes.
 */
export class DataParser {

    // Data store to populate with parsed objects
    private readonly dataStore: DataStore;

    /**
     * Initialise a new DataParser instance.
     */
    constructor() {
        this.dataStore = new DataStore();
    }

    /**
     * Parse the input raw JSON into objects and store within the
     * current DataStore instance.
     * 
     * @param rawJson JSON of data.json file.
     */
    public loadData(rawJson: JsonObject): DataStore {
        this.recurse(rawJson, null, null, 0);
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
        if(!current["name"]) {
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
        const groupID: string = (parentGroup != null) ? (parentGroup.id + "." + depth) : depth.toString();
        const dataGroup: DataGroup = new DataGroup(groupName, groupID, currentStack);

        // Store parent (if not root)
        if(parentGroup === null || parentGroup === undefined) {
            this.dataStore.addGroup(dataGroup);
        } else {
            dataGroup.parentGroup = parentGroup;
            parentGroup.subGroups.push(dataGroup);
        }

        // Store optional expansion state
        if(current["expanded"] != null) {
            dataGroup.defaultExpanded = current["expanded"] as boolean;
        }

        // Parse sources and layers (if present)
        if(current["sources"]) {
            this.parseDataSources(current["sources"] as JsonArray, dataGroup);
        }   
        if(current["layers"]) {
            this.parseDataLayers(current["layers"] as JsonArray, dataGroup);
        }

        // Add tree icon (if set)
        if(current["tree-icon"]) {
            dataGroup.treeIcon = current["tree-icon"] as string;
        }

        // Recurse into sub groups (if present)
        if(current["groups"]) {
            const groupArray = current["groups"] as JsonArray;

            for(let i = 0; i < groupArray.length; i++) {
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
    private parseDataSources(sourceArray: JsonArray, dataGroup: DataGroup) {
        for(const element of sourceArray) {
            
            const sourceID = dataGroup.id + "." + (element["id"] as string);
            const source = new DataSource(
                sourceID,
                element["type"] as string,
                dataGroup.stackEndpoint,
                element
            );

            dataGroup.dataSources.push(source);
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
        for(const element of layerArray) {
            const elementID = element["id"] as string;

            // Get matching source, ensure exists
            const originalSourceID = (element["source"] as string);
            const uniqueSourceID = this.recurseUpForSource(dataGroup, originalSourceID);
            
            const sourceObj = dataGroup.getFirstSourceWithID(uniqueSourceID);
            if(sourceObj == null) {
                console.error("Layer with ID '" + elementID + "' references a source that is not defined, will skip it!");
                continue;
            }

            // Create concrete class for data layer
            let layer: DataLayer;
            const layerID = dataGroup.id + "." + elementID;

            switch(window.type.toLowerCase()) {
                case "mapbox": 
                    layer = new MapboxDataLayer(
                        layerID,
                        element["name"] as string,
                        sourceObj,
                        element
                    );
                break;

                case "cesium": 
                    throw new Error("Not yet implemented.");

                default: 
                    throw new Error("Unknown map provider type, stopping execution.");
            }

            // Add order number (if set)
            if(element["order"] != null) {
                layer.order = element["order"] as number;
            }

            // Cache visibility & interaction level
            this.setVisibility(element, layer);
            this.setInteractions(element, layer);

            // Store the layer
            dataGroup.dataLayers.push(layer);
        }
    }

    /**
     * 
     * @param element 
     * @param layer 
     */
    private setVisibility(element: JsonObject, layer: DataLayer) {
        const layoutObj = element["layout"] as JsonObject;
        if(layoutObj?.["visibility"] != null) {
            layer.cachedVisibility = (layoutObj["visibility"] == "visible");
        } else if(element["visibility"] != null) {
            // Support older format of this property
            layer.cachedVisibility = (element["visibility"] == "visible");
        }
    }

    /**
     * 
     * @param element 
     * @param layer 
     */
    private setInteractions(element: JsonObject, layer: DataLayer) {
        if(element["interactions"] != null) {
            layer.interactions = element["interactions"] as string;
        } else if(element["clickable"] != null) {
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
        for(const source of dataGroup.dataSources) {
            const sourceDef = source.definition;
            if((sourceDef["id"] as string) === sourceID) return source.id;
        }
        return this.recurseUpForSource(dataGroup.parentGroup, sourceID);
    }

}
// End of class.