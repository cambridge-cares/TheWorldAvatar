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
     * Clears the data store.
     */
    public reset() {
        this.dataGroups = [];
    }

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
     * Recursively find and load all DataGroups defined within the data.json
     * file.
     * 
     * @param dataJSON optional pre-read config in JSON form
     * 
     * @returns Promise that fulfills when all loading is complete
     */
    public loadDataGroups(dataJSON) {
        if(dataJSON == null) {
            let self = this;
            
            return $.getJSON("./data.json", function(json) {
                self.recurseLoadDataGroups(json, null, null, self.dataGroups.length);
            }).fail((error) => {
                throw error;
            });    
        }

        return new Promise((resolve, reject) => {
            this.recurseLoadDataGroups(dataJSON, null, null, this.dataGroups.length);
            resolve("done");
        });
    }

    /**
     * Recursively parses the visualisation definition file into hierarchal
     * DataGroup instances.
     */
    private recurseLoadDataGroups(currentNode: Object, parentGroup: DataGroup, currentStack: string, groupID: number) {
        if(!currentNode["name"]) {
            throw new Error("Cannot parse a DataGroup that has no name!")
        }

        if(currentNode["stack"]) {
            // Update the stack URL to replace localhost addresses with the
            // current host location.
            currentStack = updateURL(currentNode["stack"]);
        }

        // Initialise data group
        let dataGroup = new DataGroup();
        dataGroup.name = currentNode["name"];
        dataGroup.id = groupID.toString();
        if(parentGroup !== null) {
            dataGroup.id = parentGroup.id + "." + groupID;
        }

        // Store parent (if not root)
        if(parentGroup === null || parentGroup === undefined) {
            this.dataGroups.push(dataGroup);
        } else {
            dataGroup.parentGroup = parentGroup;
            parentGroup.subGroups.push(dataGroup);
        }
        
        // Store optional expansion state
        if(currentNode.hasOwnProperty("expanded")) dataGroup.defaultExpanded = currentNode["expanded"];

        // Parse sources and layers (if present)
        if(currentNode["sources"]) {
            dataGroup.parseDataSources(currentNode["sources"]);
        }   
        if(currentNode["layers"]) {
            dataGroup.parseDataLayers(currentStack, currentNode["layers"]);
        }

        // Recurse into sub groups (if present)
        if(currentNode["groups"]) {
            for(var i = 0; i < currentNode["groups"].length; i++) {
                let subNode = currentNode["groups"][i];
                this.recurseLoadDataGroups(subNode, dataGroup, currentStack, i);
            }
        }
    }

    /**
     * Returns the layer with the input ID from anywhere in the store.
     */
    public getLayerWithID(id: string): DataLayer {
        for(var i = 0; i < this.dataGroups.length; i++) {
            let result = this.dataGroups[i].getLayerWithID(id);
            if(result !== null) return result;
        }
        return null;
    }

    /**
     * Gets a flatten list of all MapLayer instances defined across the entire
     * group hierarchy.
     */
    public getLayerList() {
        let layers = [];
        this.dataGroups.forEach(topGroup => {
            this.collectLayers(layers,topGroup);
        })
        return layers;
    }

    /**
     * Recurse through group hierarchy, adding any present layer objects to the
     * flattened layerList parameter.
     * 
     * @param layerList collector for flatten layers.
     * @param currentGroup current data group in hierarchy.
     */
    private collectLayers(layerList, currentGroup) {
        if(currentGroup != null) {
            layerList.push(...currentGroup.dataLayers);
    
            currentGroup.subGroups.forEach(subGroup => {
                this.collectLayers(layerList, subGroup);
            });
        }
    }

}