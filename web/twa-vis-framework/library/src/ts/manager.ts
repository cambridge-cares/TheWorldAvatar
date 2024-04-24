/**
 * Sore manager class for TWA-VF visualisations.
 * 
 * An instance of this class should have been created and setup for global access
 * under the "manager" variable name, within the <script> element of the visualisation's
 * main HTML file.
 */
class Manager {

    /**
     * Enum for map provider. 
     */
    public static PROVIDER: MapProvider;

    /**
     * Stores definitions of data sources.
     */
    public static DATA_STORE: DataStore = new DataStore();

    /**
     * Layer names keyed by the stack URLs they came from
     */
    public static STACK_LAYERS = {};

    /**
     * Global visualisation settings.
     */
    public static SETTINGS: Settings;

    /**
     * Map handler instance.
     */
    private mapHandler: MapHandler;
   
    /**
     * Handles map controls (on the left).
     */
    private controlHandler: ControlHandler;

    /**
     * Handles the side panel (on the right).
     */
    private panelHandler: PanelHandler;

    /**
     * Handles feature searching.
     */
    private searchHandler: SearchHandler;

    /**
     * Handles scenario selection.
     */
    private scenarioHandler: ScenarioHandler;

    /**
     * Optional callbacks to trigger once a singluar feature has been selected.
     */
    public selectionCallbacks = [];

    /**
     * Optional callbacks to trigger once a feature selection is cleared.
     */
    public unselectionCallbacks = [];

    /**
     * Optional callback to trigger once data definitions have been loaded.
     */
    public dataLoadCallback;

    /**
     * Initialise a new Manager instance.
     */
    constructor(mapProvider: MapProvider) {
        Manager.PROVIDER = mapProvider;
        this.controlHandler = new ControlHandler();
        this.panelHandler = new PanelHandler(this);

        // Initialise the map handler instance
        switch(mapProvider) {
            case MapProvider.MAPBOX:
                this.mapHandler = new MapHandler_Mapbox(this);
            break;

            case MapProvider.CESIUM:
                this.mapHandler = new MapHandler_Cesium(this);
            break;

            default:
                throw new Error("Unknown map provider specified!");
        }
    }

    /**
     * Reads Mapbox credentials from files/docker secrets.
     */
    public async readCredentials() {
        // Enter Mapbox account name and API key here!
        await $.get("mapbox_username", {}, function (result) {
            MapHandler.MAP_USER = result;
        }).fail(function () {
            console.error("Could not read Mapbox username from 'mapbox_username' secret file.");
        });
        await $.get("mapbox_api_key", {}, function (result) {
            MapHandler.MAP_API = result;
        }).fail(function () {
            console.error("Could not read Mapbox API key from 'mapbox_api_key' secret file.");
        });

        console.log("Credentials have been read from file.");
    }

    /**
     * Initialise the (blank) map instance via the map handler.
     */
    public initialiseMap() {
        // Initialise the map
        let mapOptions = Manager.SETTINGS.getSetting("start");
        this.mapHandler.initialiseMap(mapOptions);

        this.controlHandler.showControls();
        this.panelHandler.toggleMode();

        // Show attributions if present
        showAttributions();

        // Build dashboard button if appropriate
        buildDashboardButton();

        // Listen for CTRL+F 
        let self = this;
        document.addEventListener("keydown", function(e){
            if ((e.ctrlKey || e.metaKey) && e.key === "f") {
                if(self.searchHandler === null || self.searchHandler === undefined) {

                    // Initialise the seach handler instance
                    switch(Manager.PROVIDER) {
                        case MapProvider.MAPBOX:
                            self.searchHandler = new SearchHandler_Mapbox();
                        break;

                        case MapProvider.CESIUM:
                            // NOT YET IMPLEMENTED
                        break;
                    }
                }

                if(self.searchHandler != null) self.searchHandler.toggle();
                e.preventDefault();

            } else if ((e.ctrlKey || e.metaKey) && e.altKey && e.key === "t") {
                if(Manager.PROVIDER === MapProvider.CESIUM) {
                    console.log("Camera Longitude: " + Cesium.Math.toDegrees(MapHandler.MAP.camera.positionCartographic.longitude));
                    console.log("Camera Latitude: " + Cesium.Math.toDegrees(MapHandler.MAP.camera.positionCartographic.latitude));
                    console.log("Camera Height: " + MapHandler.MAP.camera.positionCartographic.height);
                    console.log("Camera Heading: " + Cesium.Math.toDegrees(MapHandler.MAP.camera.heading));
                    console.log("Camera Pitch: " + Cesium.Math.toDegrees(MapHandler.MAP.camera.pitch));
                    console.log("Camera Roll: " + Cesium.Math.toDegrees(MapHandler.MAP.camera.roll));
                }

                e.preventDefault();
            }
        });

    }

    /**
     * Adds a callback that will fire with the selected feature once a 
     * singular feature has been selected.
     * 
     * @param selectionCallback callback function.
     */
    public addSelectionCallback(selectionCallback) {
        this.selectionCallbacks.push(selectionCallback);
    }

    /**
     * Adds a callback that will fire with no parameters once the
     * current feature selection is cleared.
     * 
     * @param unselectionCallback callback function.
     */
    public addUnselectionCallback(unselectionCallback) {
        this.unselectionCallbacks.push(unselectionCallback);
        this.panelHandler.addUnselectionCallback(unselectionCallback);
    }

    /**
     * Adds a callback that will fire with two arrays of layerIDs (visible, hidden)
     * when any selection state in the layer tree changes.
     * 
     * @param treeSelectionCallback callback function.
     */
    public addTreeSelectionCallback(treeSelectionCallback) {
        this.controlHandler.addTreeSelectionCallback(treeSelectionCallback);
    }

    /**
     * Loads the global (i.e. non-data specific) visualisation settings.
     * 
     * @returns promise object
     */
    public loadSettings() {
        Manager.STACK_LAYERS = {};
        Manager.SETTINGS = new Settings();
        
        return Manager.SETTINGS.loadSettings("./settings.json").then(() => {
            let enabled = (Manager.SETTINGS.getSetting("search") != null);
            let searchIcon = document.getElementById("searchIconContainer");
            if(searchIcon != null) searchIcon.style.display = (enabled) ? "block" : "none";
            console.log("Map configuration settings have been loaded.");
        });
    }

    /**
     * Loads data definitions from the local 'data.json' file and 
     * global settings from the local 'settings.json' file.
     * 
     * @deprecated The loadSettings() and loadDefinitionsFromURL() functions should be used.
     * 
     * @return promise object
     */
    public loadDefinitions() {
        let settingPromise = this.loadSettings();
    
        return settingPromise.then(() => {
            if(!this.checkForScenarios()) {
                return this.loadDefinitionsFromURL("./data.json");
            }
        });
    }

    /**
     * Loads the data configuration from the input JSON Object.
     * 
     * @param dataJSON JSON Object of configuration.
     * 
     * @returns promise object
     */
    public loadDefinitionsFromObject(dataJSON) {
        if(dataJSON == null) {
            return Promise.resolve();
        }

        // Initialise global settings
        Manager.DATA_STORE.reset();

        let promise =  Manager.DATA_STORE.loadDataGroups(dataJSON) as Promise<any>;
        return promise.then(() => {
            // Rebuild the layer tree
            this.controlHandler.rebuildTree(Manager.DATA_STORE);
            if(this.dataLoadCallback != null) this.dataLoadCallback();
        });
    }

    /**
     * Loads the data configuration from a URL.
     * 
     * @param dataURL configuration file URL.
     * 
     * @returns promise object
     */
    public loadDefinitionsFromURL(dataURL) {
        let self = this;
        let promise = $.getJSON(dataURL, function(json) {
            return json;
        }).fail((error) => {
            console.log("Error reading data specification file from URL.");
            console.log(error);
        });    

        return promise.then((response) => self.loadDefinitionsFromObject(response));
    }

    /**
     * Loads custom image and link content.
     * 
     * @returns Promise object, resolves when all loading is complete.
     */
    public loadImagesAndLinks() {
        let promises = [];

        // Load images
        if(Manager.PROVIDER === MapProvider.MAPBOX) {
            let iconFile = "./icons.json";
            let iconPromise = (<MapHandler_Mapbox> this.mapHandler).addIcons(iconFile);
            promises.push(iconPromise);
        }

        // Load links
        let linksFile = "./links.json";
        promises.push(this.panelHandler.addLinks(linksFile));

        // Return combined promise
        return Promise.allSettled(promises);
    }
    
    /**
     * Get the map handler instance.
     */
     public getMapHandler() {
        return this.mapHandler;
    }

    /**
     * Get the side panel handler instance.
     */
    public getPanelHandler() {
        return this.panelHandler;
    }

    /**
     * Get the control handler instance.
    */
    public getControlHandler() {
        return this.controlHandler;
    }
    
    /**
     * Given an array of hierarchal group names, find the data group that
     * corresponds to it and plot it. If no group names are passed, the
     * default group is used.
     */
    public plotData() {
        this.mapHandler.plotData(Manager.DATA_STORE);
    }

    /**
     * Fires when the drop down list of overlapping features has its
     * selection changed.
     */
    public async featureSelectChange(select: HTMLInputElement) {
        if(window.selectFeatures !== null && window.selectFeatures !== undefined) {
            let feature = window.selectFeatures[select.value];
            this.showFeature(feature, feature["properties"]);
        } else {
            console.error("Could not find feature cached with key: " + select.value);
        }
        
        // Clear cache
        window.selectFeatures = {};
    }

    /**
     * Fires when an individual feature is selected.
     */
    public showFeature(feature, properties) {
        // Bug out if no properties at all
        if((properties === null || properties === undefined) && feature["properties"] != null) {
            properties = feature["properties"];
        } else if(properties === null) {
            console.warn("Selected feature has no properties, cannot show any side panel content!");
            return;
        }

        // Get the correct name for the feature
        let name = getName(properties);
        if(name == null) {
            if(feature.hasOwnProperty("id") && typeof feature["id"] !== "object") {
                name = "Feature " + feature["id"];
            } else {
                name = "Selected Feature";
            }
        }
        this.panelHandler.setTitle("<h3>" + name + "</h2");
        document.getElementById("titleContainer").classList.add("clickable");

        // Description
        let desc = properties["description"];
        if(desc === null && properties["desc"]) {
            desc = properties["desc"];
        }
        if(desc !== null && desc !== undefined) {
            this.panelHandler.setContent("<div class='description'>" + desc + "</div>");
        } else {
            this.panelHandler.setContent("");
        }

        // Retrieve and display meta and time series data
        let scenarioID = (this.scenarioHandler == null) ? null : this.scenarioHandler.selectedScenario;
        this.panelHandler.addSupportingData(feature, properties, scenarioID);

        // Update buttons accordingly
        let metaTreeButton = document.getElementById("treeButton");
        let timeseriesButton = document.getElementById("timeButton");

        if(desc === undefined && metaTreeButton === null && timeseriesButton === null) {
            // Add label that there's no data
            this.panelHandler.setContent(
                "<div class='description'>No data is available for this location.</div>"
            );
        } 
        
        // Simulate click on general tab
        // @ts-ignore
        $("#sidePanelInner").tabs("option", "active", 0);

        // Show return button
        document.getElementById("returnContainer").style.display = "table";
        
        // Store selected feature
        window.currentFeature = feature;

        // Fire selection callbacks
        this.selectionCallbacks.forEach(callback => {
            callback(feature);
        });
    }

    /**
     * Programatically select the metadata or timeseries tabs.
     * 
     * @param {String} tabButtonName 
     * @param {String} tabName 
     */
    openMetaTab(tabButtonName, tabName) {
        // Declare all variables
        var i, tabcontent, tablinks;
      
        // Get all elements with class="tabcontent" and hide them
        tabcontent = document.getElementsByClassName("tabcontent");
        for (i = 0; i < tabcontent.length; i++) {
            tabcontent[i].style.display = "none";
        }
      
        // Get all elements with class="tablinks" and remove the class "active"
        tablinks = document.getElementsByClassName("tablinks");
        for (i = 0; i < tablinks.length; i++) {
            tablinks[i].className = tablinks[i].className.replace(" active", "");
        }
      
        // Show the current tab, and add an "active" class to the button that opened the tab
        document.getElementById(tabName).style.display = "block";
        document.getElementById(tabButtonName).className += " active";
    }

    /**
     * Update the time series charts following a change in 
     * the selected series.
     */
    public updateTimeseries(setName) {
        this.panelHandler.updateTimeseries(setName);
    }

    /**
     * Ease the map to pan and zoom to the currently selected feature.
     */
    public moveMapToFeature() {
        let titleContainer = document.getElementById("titleContainer");
        if(titleContainer.classList.contains("clickable")) {

            let target = window.currentFeature;
            if(target == null) return;

            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    target = getCenter(target);

                    MapHandler.MAP.easeTo({
                        center: target,
                        zoom: 16,
                        duration: 3000,
                        essential: true
                    });
                break;

                case MapProvider.CESIUM:
                   CesiumUtils.flyToFeature(target);
                break;
            }
        };
    }

    /**
     * Move the map to the coords specified in the info panel.
     */
    public moveMap() {
        let lng = (document.getElementById("lngCell") as HTMLInputElement).value;
		let lat = (document.getElementById("latCell") as HTMLInputElement).value;

        let target = [lng, lat];
        MapHandler.MAP.jumpTo({
            center: target
        });

        this.controlHandler.editingCoords = false;
        this.controlHandler.updateInfoPanel(null);
    }

    /**
     * Clear the current feature finder seach (BETA).
     */
    public cancelSearch() {
        // Reset to previous filters
        let rootGroups = Manager.DATA_STORE.dataGroups;
        rootGroups.forEach(rootGroup => {

            let layers = rootGroup.flattenDown();
            layers.forEach(layer => {

                // Get the source
                let source = layer.source;

                // Check and (if needed) re-enable clustering
                let style = MapHandler.MAP.getStyle();
                if(source.definition["cluster"]) {
                    if(!style.sources[source.id].cluster) {
                        style.sources[source.id].cluster = true;
                        MapHandler.MAP.setStyle(style);
                    }
                }

                // Reset layer to old filter
                let oldFilter = layer.definition["filter"];
                if(oldFilter !== null && oldFilter !== undefined) {
                    MapHandler.MAP.setFilter(layer.id, oldFilter);
                } else {
                    MapHandler.MAP.setFilter(layer.id, null);
                }
            });
        });
    }

    /**
     * Update the layer filters for the feature finder after an
     * update to the search term (BETA).
     */
    public updateSearch(textField: HTMLInputElement) {
        let searchTerm = textField.value.toLowerCase();

        let rootGroups = Manager.DATA_STORE.dataGroups;
        rootGroups.forEach(rootGroup => {

            let layers = rootGroup.flattenDown();
            layers.forEach(layer => {

                // Get the source
                let source = layer.source;

                // Build filter
                let searchFilter = [
                    "all",
                    ["has", "name"],
                    ["in", searchTerm, ["downcase", ["get", "name"]]]
                ];

                // Combine with existing feature
                let newFilter = searchFilter;
                let oldFilter = layer.definition["filter"];

                if(oldFilter !== null && oldFilter !== undefined) {
                    // Clustering does not work well with this search feature, so this is how it's handled
                    // During the time the search is active:
                    //      - Layers that only show clustered locations are disabled using a restrictive filter
                    //      - Layers that omit clustered locations temporarily show everything
                    //      - Sources that have clustering enabled will have it disabled
                    //      - Other layers have their existing filters combined with the search filter

                    if(this.filterMatch(oldFilter, ["has", "point_count"]) || this.filterMatch(oldFilter, ["has", "point_count_abbreviated"])) {
                        // This is a layer of clustered locations, temporarily disable it
                        newFilter = ["has", "nonsense-string"];

                        // Disable clustering on the source
                        let style = MapHandler.MAP.getStyle();
                        style.sources[source.id].cluster = false;
                        MapHandler.MAP.setStyle(style);

                    } else if(this.filterMatch(oldFilter, ["!", ["has", "point_count"]]) || this.filterMatch(oldFilter, ["!", ["has", "point_count_abbreviated"]])) {
                        // This is a layer that omits clustered locations, temporarily show all points by not combining
                        newFilter = searchFilter;

                    } else {
                        // Add filters together
                        newFilter = ["all", oldFilter, newFilter];
                    }
                }

                // Apply filter
                if(newFilter !== null) MapHandler.MAP.setFilter(layer.id, newFilter);
            });
        });
    }

     /**
     * Open the feature search controls.
     */
     public openSearch() {
        if(Manager.SETTINGS.getSetting("search") == null) return;

        if(this.searchHandler === null || this.searchHandler === undefined) {
            // Initialise the seach handler instance
            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    this.searchHandler = new SearchHandler_Mapbox();
                break;
            }
        }

        if(this.searchHandler != null) this.searchHandler.toggle();
    }

    /**
     * Do input filters match?
     */
    private filterMatch(filterOne, filterTwo) {
        let one = JSON.stringify(filterOne);
        let two = JSON.stringify(filterTwo);
        return one === two;
    }

    /**
     * Given a selected feature, this function trys to determine the id of the layer
     * containing it. If found, this is then used to find the original group housing
     * the layer and then the stack URL attached to this group.
     * 
     * Note: this is bloated as Cesium does not have a common abstraction for feature
     * objects, each has its own annoying structure.
     * 
     * @param feature selected geographical feature.
     *  
     * @returns stack URL (or null) 
     */
    public static findStack(feature, properties) {
        switch(Manager.PROVIDER) {
            case MapProvider.CESIUM: {

                if(feature instanceof Cesium.Cesium3DTileFeature) {
                    // Feature within 3D tileset
                    let tileset = feature.tileset;

                    if(tileset.hasOwnProperty("layerID")) {
                        let layerID = tileset["layerID"];

                        for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                            let layers = value as string[];
                            if(layers.includes(layerID)) {
                                return stack;
                            } 
                        }
                    } else {
                        // No way to determine what layer this feature came from
                        return null;
                    }
                } else if(feature.hasOwnProperty("primitive") && feature["primitive"] instanceof Cesium.Cesium3DTileset) {
                     // Feature within 3D tileset, for some reason using a different Cesium data object?
                     let tileset = feature.primitive;

                    if(tileset.hasOwnProperty("layerID")) {
                        let layerID = tileset["layerID"];

                        for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                            let layers = value as string[];
                            if(layers.includes(layerID)) {
                                return stack;
                            } 
                        }
                    } else {
                        // No way to determine what layer this feature came from
                        return null;
                    }
                } else if(feature instanceof Cesium.ImageryLayerFeatureInfo) {
                    // WMS feature on cesium
                    let layer = feature["imageryLayer"];
                    let provider = layer["imageryProvider"];

                    if(provider.hasOwnProperty("layerID")) {
                        let layerID = provider["layerID"];

                        for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                            let layers = value as string[];
                            if(layers.includes(layerID)) {
                                return stack;
                            } 
                        }
                    } else {
                        // No way to determine what layer this feature came from
                        return null;
                    }

                } else {
                    // Something else, try to find the layerID
                    let entity = feature["id"];

                    if(entity !== null && entity !== undefined) {
                        let collection = entity["entityCollection"];
                        let owner = collection.owner;

                        if(owner !== null && owner !== undefined && owner.hasOwnProperty("layerID")) {
                            let layerID = owner["layerID"];

                            for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                                let layers = value as string[];
                                if(layers.includes(layerID)) {
                                    return stack;
                                } 
                            }
                        }
                    }
                }
            }
            break;

            case MapProvider.MAPBOX: {
                // Mapbox
                let layer = feature["layer"]["id"];

                if(layer !== null && layer !== undefined) {
                    for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                        let layers = value as string[];
                        if(layers.includes(layer)) return stack;
                    }
                }
            }
            break;
        }

        return null;
    }

    /**
     * Checks if scenarios have been enabled for this visualisation.
     * 
     * @returns true if scenarios enabled, false if not 
     */
    public checkForScenarios() {
        let scenarioButton = document.getElementById("scenarioChangeContainer");

        // If a scenario endpoint is specified, load the handler
        let scenarioURL = Manager.SETTINGS.getSetting("scenarioAgent");
        let scenarioDataset = Manager.SETTINGS.getSetting("scenarioDataset");

        if(this.scenarioHandler == null && scenarioURL != null) {
            this.scenarioHandler = new ScenarioHandler(scenarioURL, scenarioDataset);
            scenarioButton.style.display = "block";
            return true;
        } 
        return scenarioURL != null;
    }

    /**
     * Displays the scenario selector component.
     */
    public showScenarioSelector() {
        if(this.scenarioHandler != null) {
            this.scenarioHandler.showSelector();
        }
    }

    /**
     * Selects and loads data from the input scenario.
     * 
     * @param scenarioID scenario id 
     * @param scenarioName user facing name of the scenario
     */
    public selectScenario(scenarioID, scenarioName) {
        if(this.scenarioHandler != null) {
            if(scenarioID === this.scenarioHandler.selectedScenario) return;

            // Set the selected scenario
            this.scenarioHandler.selectScenario(scenarioID, scenarioName);

            // Show the current name
            let container = document.getElementById("currentScenarioName");
            container.innerHTML = "Current: " + scenarioName;

            // Load its data configuration file
            let self = this;
            this.scenarioHandler.getConfiguration(function(dataJSON) {
                let promise = self.loadDefinitionsFromObject(dataJSON) as Promise<any>;
                promise.then(() => self.plotData());
            });
        }
    }
}