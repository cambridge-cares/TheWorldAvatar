/**
 * Sore manager class for DVTF visualisations.
 * 
 * An instance of this class should have been created and setup for global access
 * under the "manager" variable name.
 * 
 * TODO: Move out the feature finder functionality to a better (or new) class.
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
     * Handles controls on left.
     */
    private controlHandler: ControlHandler;

    /**
     * Handles the side panel.
     */
    private panelHandler: PanelHandler;

    /**
     * Is the feature search bar currently up?
     */
    private searchUp: boolean = false;

    /**
     * Currently in full screen mode?
     */
    private inFullscreen: boolean = false;

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
                this.mapHandler = new MapHandler_Mapbox(this);
            break;

            case MapProvider.CESIUM:
                this.mapHandler = new MapHandler_Cesium(this);
            break;

            default:
                throw new Error("Unknown map provider specified!");
            break;
        }
    }

    /**
     * Initialise the (blank) map instance via the map handler.
     */
    public initialiseMap() {
        // Initialise the map
        let mapOptions = Manager.SETTINGS.getSetting("start");
        this.mapHandler.initialiseMap(mapOptions);

        this.controlHandler.showControls();
        this.controlHandler.rebuildTree(Manager.DATA_STORE);

        this.panelHandler.toggleMode();

        // Override CTRL+F shortcut for feature searching (BETA)
        let searchBox = document.getElementById("finderContainer");
        if(searchBox !== null) {

            let self = this;
            document.addEventListener("keydown", function(e){
                if (Manager.PROVIDER === MapProvider.MAPBOX && (e.ctrlKey || e.metaKey) && e.key === "f") {
                    if(self.searchUp) {
                        self.hideSearch();
                    } else {
                        self.showFeatureFinder();
                    }
                    e.preventDefault();
                }

                if(e.altKey && e.key === "Enter") {
                    self.toggleFullscreen();
                    
                    var ellipsoid = MapHandler.MAP.scene.globe.ellipsoid;
                    var cartographic = ellipsoid.cartesianToCartographic(MapHandler.MAP.camera.position);
                    // @ts-ignore
                    var longitudeString = Cesium.Math.toDegrees(cartographic.longitude).toFixed(10);
                    // @ts-ignore
                    var latitudeString = Cesium.Math.toDegrees(cartographic.latitude).toFixed(10);
                }
            });
        }
    }

    private toggleFullscreen() {
        if(this.inFullscreen) {
            // Disable full screen
            document.getElementById("controlsContainer").style.display = "block";
            document.getElementById("sidePanel").style.display = "block";

            let sidePanel =  document.getElementById("sidePanel");

            if(sidePanel.classList.contains("large")) {
                document.getElementById("map").style.width = "100%";
            } else if(sidePanel.classList.contains("collapsed")) {
                document.getElementById("map").style.width = "calc(100% - 28px)";
            } else {
                document.getElementById("map").style.width = "calc(100% - 500px)";
            }

            this.inFullscreen = false;
        } else {
            // Enable full scrren
            document.getElementById("controlsContainer").style.display = "none";
            document.getElementById("sidePanel").style.display = "none";
            document.getElementById("map").style.width = "100%";
            this.inFullscreen = true;
        }

        if(Manager.PROVIDER === MapProvider.MAPBOX) {
            MapHandler.MAP.resize();
        } else {
            MapHandler.MAP.scene.requestRender();
        }
    }

    /**
     * Loads the definition of data groups and the global visualisation settings.
     * 
     * @returns promise object
     */
    public loadDefinitions() {
        Manager.STACK_LAYERS = {};
        let promises = [];

        // Initialise global settings
        Manager.SETTINGS = new Settings();
        promises.push(Manager.SETTINGS.loadSettings("./settings.json"));

        // Load data definitions
        promises.push(Manager.DATA_STORE.loadDataGroups("./data.json"));

        return Promise.all(promises);
    }

    /**
     * 
     */
    public loadImagesAndLinks() {
        let promises = [];

            if(Manager.PROVIDER === MapProvider.MAPBOX) {
            let iconFile = "./icons.json";
                let iconPromise = (<MapHandler_Mapbox> this.mapHandler).addIcons(iconFile);
                promises.push(iconPromise);
            }

        let linksFile = "./links.json"
            promises.push(this.panelHandler.addLinks(linksFile));

        let promise = Promise.all(promises).catch(function(err) {
            console.warn("Loading icons and/or links has failed, these will be skipped."); 
        });
        return promise;
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
        let name = getName(properties);

        if(name == null) {
            if(feature.hasOwnProperty("id")) {
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

        // Retrieve and display meta and timeseries data
        this.panelHandler.addSupportingData(feature, properties);

        // Update buttons accordingly
        let metaTreeButton = document.getElementById("treeButton");
        let timeseriesButton = document.getElementById("timeButton");

        if(desc === undefined && metaTreeButton === null && timeseriesButton === null) {
            // Add label that there's no data
            this.panelHandler.setContent(
                "<div class='description'>No data is available for this location.</div>"
            );
        } else {
            // Simulate click on meta button
            if(metaTreeButton !== null) metaTreeButton.click();
        }

        // Simulate click on general tab
        // @ts-ignore
        $("#sidePanelInner").tabs("option", "active", 0);

        // Show return button
        document.getElementById("returnContainer").style.display = "table";
        
        // Store selected feature
        window.currentFeature = feature;
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

            let target = getCenter(window.currentFeature);
            MapHandler.MAP.easeTo({
                center: target,
                zoom: 16,
                duration: 3000,
                essential: true
            });
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
     * Show the feature finder panel (BETA).
     */
    public showFeatureFinder() {
        let finderContainer = document.getElementById("finderContainer");
        let sidePanel = document.getElementById("sidePanel");

        // No feature if side panel in large mode
        if(sidePanel.classList.contains("large")) return;

        // Adjust for current width state
        if(sidePanel.classList.contains("expanded")) {
            finderContainer.classList.remove("collapsed");
            finderContainer.classList.add("expanded");
        } else {
            finderContainer.classList.remove("expanded");
            finderContainer.classList.add("collapsed");
        }

        finderContainer.style.display = "block";
        this.searchUp = true;

        document.getElementById("findInput").focus();
    }

    /**
     * Hide the feature finder panel (BETA).
     */
    public hideSearch() {
        let finderContainer = document.getElementById("finderContainer");
        finderContainer.style.display = "none";
        this.searchUp = false;
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

        // Hide search bar
        let finderContainer = document.getElementById("finderContainer");
        finderContainer.style.display = "none";
        this.searchUp = false;
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
     * Do input filters match?
     */
    private filterMatch(filterOne, filterTwo) {
        let one = JSON.stringify(filterOne);
        let two = JSON.stringify(filterTwo);
        return one === two;
    }

    /**
     * 
     * @param feature 
     * @returns 
     */
    public static findStack(feature, properties) {
        // @ts-ignore
        if(feature instanceof Cesium.Cesium3DTileFeature) {
            // Feature within 3D tileset
            let tileset = feature.tileset;

        } else if(feature instanceof Cesium.ImageryLayerFeatureInfo) {
            // WMS feature on cesium
            return null;

        } else {
            // Mapbox or WMS feature?
            let layer = feature["layer"]["id"];

            if(layer !== null && layer !== undefined) {

                for (let [stack, value] of Object.entries(Manager.STACK_LAYERS)) {
                    let layers = value as string[];
                    if(layers.includes(layer)) return stack;
                }
            }
        }

        return null;
    }
}