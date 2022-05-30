/**
 * 
 */
class Manager {

    /**
     * Group currently being plotted.
     */
    public static CURRENT_GROUP: DataGroup;

    /**
     * 
     */
    public static PROVIDER: MapProvider;

    /**
     * Stores definitions of data sources.
     */
    public dataStore: DataStore = new DataStore();

    /**
     * Map handler instance.
     */
    private mapHandler: MapHandler;
   
    /**
     * Handles controls on left.
     */
    private controlHandler: ControlHandler;

    /**
     * 
     */
    private panelHandler: PanelHandler;

    /**
     * Initialise a new Manager instance.
     */
    constructor(mapProvider: MapProvider) {
        Manager.PROVIDER = mapProvider;
        this.controlHandler = new ControlHandler();
        this.panelHandler = new PanelHandler();

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
     * 
     */
    public onLayerChange(control: Object) {
        //this.controlHandler.onLayerChange(control);
    }

    /**
     * 
     */
    public getPanelHandler() {
        return this.panelHandler;
    }

    /**
     * Initialise the (blank) map instance via the map handler.
     * 
     * @param mapOptionsOverride dictionary of default map options. If passed this will be used
     * when initialising the map rather than any meta data stored within DataGroups.
     */
    public initialiseMap(mapOptions: Object) {
        // Initialise the map

        if(mapOptions === null || mapOptions === undefined) {
            // Try to pick up map options from the first listed stack
            let firstRoot = this.dataStore.dataGroups[0];
            if(firstRoot.mapOptions !== null) {
                mapOptions = firstRoot.mapOptions;
            }
        }

        this.mapHandler.initialiseMap(mapOptions);
        this.controlHandler.showControls();
        this.controlHandler.rebuildTree(this.dataStore);

        this.panelHandler.toggleMode();
    }

    /**
     * Given the location of one (or more) visualisation files, query and parse
     * them all into object definitions. 
     * 
     * @param visFiles visualisation file URLs
     * 
     * @returns promise object
     */
    public loadDefinitions(visFiles: string[]) {
        let promises = [];

        visFiles.forEach(visFile => {
            promises.push(this.dataStore.loadDataGroups(visFile));
        })

        return Promise.all(promises);
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
     */
    public plotData() {
        this.mapHandler.plotData(this.dataStore);
    }
}