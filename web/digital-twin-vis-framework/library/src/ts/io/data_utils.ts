/**
 * Utilities for definition data.
 */
class DataUtils {

    /**
     * Given a JSON object defining a single data set, this method
     * updates the type definitions to the newer format.
     * 
     * @param setJSON json defining a single data set
     */
    public static updateTypes(setJSON: Object) {

        // The old 'locationType' parameter was a mix of data type and 
        // display type, these have now been separated.
        let oldType = setJSON["locationType"];
        if(oldType !== null && oldType !== undefined) {
            switch(oldType) {
                case "circle":
                case "point":
                case "symbol":
                    setJSON["dataType"] = "point";
                    break;

                case "line":
                case "connection":
                    setJSON["dataType"] = "line";
                break;

                case "fill":
                case "polygon":
                case "building":
                case "extrusion":
                    setJSON["dataType"] = "polygon";
                break;
            }
            delete setJSON["locationType"];
        }
    }

    /**
     * Given the root of a data group structure, this method does a depth first 
     * search to return the first leaf group.
     * 
     * @param currentGroup current group
     * 
     * @returns depth-first leaf group
     */
    public static getDefaultGroup(currentGroup: DataGroup): DataGroup {
        if(currentGroup.subGroups.length > 0) {
            return DataUtils.getDefaultGroup(currentGroup.subGroups[0]);
        } else {
            return currentGroup;
        }
    }

    /**
     * Returns the root group of the input group.
     * 
     * @param currentGroup current group
     * 
     * @returns root group.
     */
    public static getRootGroup(currentGroup: DataGroup): DataGroup {
        if(currentGroup.parentGroup === null || currentGroup.parentGroup === undefined) {
            return currentGroup;
        } else {
            return DataUtils.getRootGroup(currentGroup.parentGroup);
        }
    }

    /**
     * Returns the layer from the store with the input id.
     */
    public static getLayer(dataStore: DataStore, treeID: string): DataLayer {
        let parts = [];
        if (treeID.includes(".")) {
            parts = treeID.split(".");
        } else {
            parts.push(treeID);
        }

        let group = DataUtils.findGroup(
            dataStore.dataGroups[treeID[0]],
            parts.slice(1, -1),
            0
        );
        
        let layerID = parts[-1];
        return group.getLayerWithID[layerID];
    }

    /**
     * Finds the data group.
     */
    private static findGroup(currentGroup: DataGroup, parts: number[], depth: number) {
        if(currentGroup.subGroups.length > 0 && depth < parts.length) {
            let index = parts[depth];
            return DataUtils.findGroup(currentGroup.subGroups[index], parts, depth + 1);
        } else {
            return currentGroup;
        }
    }

    /**
     * Gets all layer IDs from the store
     */
    public static getAllLayerIDs(dataStore: DataStore) {
        let layerIDs = [];
        dataStore.dataGroups.forEach(group => {
            DataUtils.getLayerIDs(layerIDs, group);
        })
        return layerIDs;
    }

    /**
     * Recursively get all layer IDs.
     */
    private static getLayerIDs(layerIDs: string[], dataGroup: DataGroup) {
        dataGroup.dataLayers.forEach(layer => {
            layerIDs.push(layer.id);
        });

        dataGroup.subGroups.forEach(subGroup => {
            DataUtils.getLayerIDs(layerIDs, subGroup);
        });
    }
}