/**
 * 
 */
class Manager {

    /**
     * Group currently being plotted.
     */
    public static CURRENT_GROUP: DataGroup;

    /**
     * Stores definitions of data sources.
     */
    private dataStore: DataStore = new DataStore();

    /**
     * Map handler instance.
     */
    private mapHandler: MapHandler;

    /**
     * Initialise a new Manager instance.
     */
    constructor(mapProvider: MapProvider) {
        // Initialise the map handler instance
        switch(mapProvider) {
            case MapProvider.MAPBOX:
                this.mapHandler = new MapHandler_MapBox();
            break;

            default:
                throw new Error("Unknown map provider specified!");
            break;
        }
    }

    /**
     * Initialise the (blank) map instance via the map handler.
     * 
     * @param mapOptionsOverride dictionary of default map options. If passed this will be used
     * when initialising the map rather than any meta data stored within DataGroups.
     */
    public initialiseMap(mapOptionsOverride: Object) {
        let mapOptions = mapOptionsOverride;

        // If data has been loaded, check if the first root node contains a
        // "mapOptions" node, if so use this as the default map setting.
        if(mapOptionsOverride === undefined && this.dataStore.dataGroups.length > 0) {
            let defaultRoot = this.dataStore.dataGroups[0];
            if(defaultRoot.groupMeta !== undefined && defaultRoot.groupMeta["mapOptions"]) {
                mapOptions = defaultRoot.groupMeta["mapOptions"];
            }
        }

        // Initialise the map
        this.mapHandler.initialiseMap(mapOptions);
    }

    /**
     * Given a directory this method parses and loads the definitions of all groups
     * and data sources defined within.
     * 
     * @param dataDir location of data directory
     * 
     * @returns promise object
     */
    public loadDataDirectory(dataDir: string) {
        return this.dataStore.loadDataGroups(dataDir);
    }

    /**
     * Returns the depth-first, leaf data group to be used as the 
     * default source of plotted data.
     */
    private getDefaultGroup(): DataGroup {
        if(this.dataStore.dataGroups.length === 0) {
            throw new Error("No data has been loaded!");
        }
        let firstRoot = this.dataStore.dataGroups[0];
        return DataUtils.getDefaultGroup(firstRoot);
    }

    /**
     * Given an array of hierarchal group names, find the data group that
     * corresponds to it and plot it. If no group names are passed, the
     * default group is used.
     * 
     * @param groupNames hierarchal name of group to plot.
     */
    public plotGroup(groupNames: string[]) {
        let group = this.getDefaultGroup();
        
        // If group names are passed, find that group
        if(groupNames !== undefined && groupNames.length > 0) {
            group = this.dataStore.getGroup(groupNames);
        }

        // Plot the group
        if(group !== null) {
            Manager.CURRENT_GROUP = group;
            this.mapHandler.plotGroup(group);
        }
    }
}