/**
 * This class handles storage of all data groups and their sources. Definitions of data sources,
 * associated meta data, and the raw data may be stored here.
 */
class DataStore {

    /**
     * Array of loaded DataGroup instances.
     */
    public dataGroups: Array<DataGroup> = [];

    /**
     * Given an array of group names, this method finds and returns the 
     * matching group (or null if not found).
     * 
     * @param groupNames array of group names
     * 
     * @return matching DataGroup (or null)
     */
    public getGroup(groupNames: string[]): DataGroup {
        let result = [];
        this.dataGroups.every(dataGroup => {
            if(result.length === 0) {
                this.recurseFindGroup(dataGroup, groupNames, 0, result);
            }
        });

        return (result.length !== 0) ? result[0] : null;
    }

    /**
     * Recurses through the data group structure the find and return the one with the matching name.
     */
    private recurseFindGroup(currentGroup: DataGroup, groupNames: string[], depth: number, result: Array<DataGroup>) {
        if(depth < 0 || currentGroup === null) return;

        let targetName = groupNames[depth];
        let self = this;

        if (targetName === currentGroup.name) {
            if(depth === (groupNames.length - 1)) {
                result[0] = currentGroup;
                return;
            } else {
                currentGroup.subGroups.forEach(dataGroup => {
                    this.recurseFindGroup(dataGroup, groupNames, depth + 1, result);
                });
            }
        }
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
    public loadDataGroups(rootDir: string) {
        let rootGroup = new DataGroup(rootDir, null);
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
        let self = this;

        // Load the JSON
        var promise = $.getJSON(metaFile, function(json) {
            return json;
        }).fail((error) => {
            console.error("Could not read invalid JSON file at " + metaFile);
        });

        // Recurse into subgroups
        return promise.then(
            function(json) {
                // Store group name
                currentGroup.name = json["name"];
                
                // Store group meta data (i.e. global)
                if(json["meta"]) {
                    currentGroup.groupMeta = json["meta"];
                }

                // Store definitions of data sources (if present)
                if(json["sources"]) {
                    currentGroup.parseDataSources(json["sources"]);
                }   

                // Store definitions of data layers (if present)
                if(json["layers"]) {
                    currentGroup.parseDataLayers(json["layers"]);
                }
              
                // Recurse into sub groups
                if(json["groups"]) {
                    currentGroup.subLabel = json["groups"]["label"];
                    let subPromises = [];

                    for(var i = 0; i < json["groups"]["directories"].length; i++) {
                        // Get sub group details
                        let subGroupDir = json["groups"]["directories"][i];
                        subGroupDir = currentDir + subGroupDir;

                        // Initialise sub group
                        let subGroup = new DataGroup(subGroupDir, currentGroup);
                        currentGroup.subGroups.push(subGroup);

                        // Step into
                        subPromises.push(self.recurseLoadDataGroups(subGroup));
                    }
                
                    return Promise.all(subPromises);
                }
            }
        );
    }
}