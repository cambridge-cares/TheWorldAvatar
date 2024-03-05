import { DataGroup } from "./data-group";
import { DataLayer } from "./data-layer";
import { DataSource } from "./data-source";

/**
 * This class handles storage of all data groups and their sources. Definitions of data sources,
 * associated meta data, and the raw data may be stored here.
 */
export class DataStore {

    /**
     * Array of loaded DataGroup instances.
     */
    private readonly _dataGroups: DataGroup[] = [];

    /**
     * Clears the data store.
     */
    public reset() {
        this._dataGroups.length = 0;
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
        const results: DataGroup[] = [];
        this._dataGroups.every(dataGroup => {
            if(results.length === 0) {
                this.recurseFindGroup(dataGroup, groupNames, 0, results);
            }
        });

        return (results.length !== 0) ? results[0] : null;
    }

    /**
     * Returns the top level data groups.
     * 
     * @returns top level data groups.
     */
    public getGroups(): DataGroup[] {
        return this._dataGroups;
    }

    /**
     * Add a top-level group to the store.
     * 
     * @param dataGroup group to add.
     */
    public addGroup(dataGroup: DataGroup) {
        this._dataGroups.push(dataGroup);
    }

    /**
     * Recurses through the data group structure the find and return the one with the matching name.
     */
    private recurseFindGroup(currentGroup: DataGroup, groupNames: string[], depth: number, result: Array<DataGroup>) {
        if(depth < 0 || currentGroup === null) return;

        const targetName = groupNames[depth];

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
     * Returns the layer with the input ID from anywhere in the store.
     */
    public getLayerWithID(layerID: string): DataLayer {
        for(const group of this._dataGroups) {
            const result: DataLayer = group.getFirstLayerWithID(layerID);
            if(result !== null) return result;
        }
        return null;
    }

    /**
     * Gets a flattened list of all DataLayer instances defined across the entire
     * group hierarchy.
     */
    public getLayerList(): DataLayer[] {
        const results: DataLayer[] = [];
        this._dataGroups.forEach(topGroup => {
            this.collectLayers(topGroup, results);
        })
        return results;
    }

    /**
     * Recurse through group hierarchy, adding any present layer objects to the
     * flattened layerList parameter.
     * 
     * @param currentGroup current data group in hierarchy.
     * @param results collector for flattened layers.
     */
    private collectLayers(currentGroup: DataGroup, results: DataLayer[]) {
        if(currentGroup != null) {
            results.push(...currentGroup.dataLayers);
    
            currentGroup.subGroups.forEach(subGroup => {
                this.collectLayers(subGroup, results);
            });
        }
    }

    /**
     * Gets a flattened list of all DataSource instances defined across the entire
     * group hierarchy.
     */
    public getSourceList(): DataSource[] {
        const results: DataSource[] = [];
        this._dataGroups.forEach(topGroup => {
            this.collectSources(topGroup, results);
        })
        return results;
    }

    /**
     * Recurse through group hierarchy, adding any present source objects to the
     * flattened results parameter.
     * 
     * @param currentGroup current data group in hierarchy.
     * @param results collector for flattened sources.
     */
    private collectSources(currentGroup: DataGroup, results: DataSource[]) {
        if(currentGroup != null) {
            results.push(...currentGroup.dataSources);
    
            currentGroup.subGroups.forEach(subGroup => {
                this.collectSources(subGroup, results);
            });
        }
    }

}