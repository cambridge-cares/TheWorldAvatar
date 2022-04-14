/**
 * This class handles storage of all data groups and their sources. Definitions of data sources,
 * associated meta data, and the raw data may be stored here.
 */
class DataStore {

    /**
     * Array of loaded DataGroup instances.
     */
    public dataGroups: Array<DataGroup> = [];

    public test() {
        this.loadDataGroups("../data").then(() => {
            console.info("--- Finished loading all sources? ---");
        });
    }
    
    /**
     * Recursively find and load all DataGroups defined within the input
     * root directory.
     * 
     * WARNING: No checking takes place to determine if this directory
     * has already been scanned and loaded.
     * 
     * @param rootDir absolute root directory to load data for
     * 
     * @returns Promise that fulfills when all loading is complete
     */
    public loadDataGroups(rootDir: String) {
        let rootGroup: DataGroup = new DataGroup("root-group", rootDir);
        this.dataGroups.push(rootGroup);

        return this.recurseLoadDataGroups(rootGroup);
    }

    /**
     * 
     * @param currentDir directory of current data group
     * @param currentGroup current data group
     * @returns 
     */
    private recurseLoadDataGroups(currentGroup: DataGroup) {
        // Look for the meta.json file in the rootDir
        let currentDir = currentGroup.location;
        currentDir = (currentDir.endsWith("/")) ? currentDir : currentDir + "/";
        let metaFile = currentDir + "meta.json";

        return $.getJSON(metaFile, function(json) {
            if(json === null) {
                throw new Error("Could not read file at " + metaFile);
            }

            let localJSON = (json["local"]) ? json["local"] : json;
            currentGroup.subLabel = localJSON["label"];

            // Store group meta data (i.e. global)
            if(json["global"]) {
                currentGroup.groupMeta = json["global"];
                if(json["global"]["data-name"]) {
                    currentGroup.name = json["global"]["data-name"];
                }
            }

            // Recurse into sub groups
            if(localJSON["groups"]) {
                let subPromises = [];

                for(var i = 0; i < localJSON["groups"].length; i++) {

                    // Get sub group details
                    let subGroupName = localJSON["groups"][i]["name"];
                    let subGroupDir = currentDir + localJSON["groups"][i]["directory"];

                    // Initialise sub group
                    let subGroup = new DataGroup(subGroupName, subGroupDir);
                    currentGroup.subGroups.push(subGroup);

                    // Step into
                    subPromises.push(this.recurseLoadDataGroups(subGroup));
                }
                console.info("Processed DataGroup instance: " + currentGroup.name);
                return Promise.all(subPromises).then(() => {
                    console.log("Loaded all subgroups from " + currentGroup.name);
                });
            }
        });
    }
}