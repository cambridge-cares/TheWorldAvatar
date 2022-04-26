/**
 * Utilities for meta data.
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
        if(currentGroup.parentGroup === null) {
            return currentGroup;
        } else {
            return DataUtils.getRootGroup(currentGroup.parentGroup);
        }
    }

    /**
     * 
     */
    public static getMapOptions(dataGroup: DataGroup): Object {
        // Get the root group
        let rootGroup = DataUtils.getRootGroup(dataGroup);

        // If it has map options, apply them
        if(rootGroup !== null && rootGroup.groupMeta !== null && rootGroup.groupMeta["mapOptions"]) {
            return rootGroup.groupMeta["mapOptions"];
        }
        return null;
    }
}