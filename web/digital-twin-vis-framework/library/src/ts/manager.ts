/**
 * 
 */
class Manager {

    private editingCoords: boolean;
    /**
     * 
     */
    public static PROVIDER: MapProvider;

    /**
     * Stores definitions of data sources.
     */
    public static DATA_STORE: DataStore = new DataStore();

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

    private endPoints: string[];

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
                this.mapHandler = new MapHandler_MapBox(this);
            break;

            default:
                throw new Error("Unknown map provider specified!");
            break;
        }
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
            let firstRoot = Manager.DATA_STORE.dataGroups[0];
            if(firstRoot.mapOptions !== null) {
                mapOptions = firstRoot.mapOptions;
            }
        }

        this.mapHandler.initialiseMap(mapOptions);
        this.controlHandler.showControls();
        this.controlHandler.rebuildTree(Manager.DATA_STORE);

        this.panelHandler.toggleMode();
        this.showInfoPanel();
    }

    /**
     * Given the location of one (or more) visualisation files, query and parse
     * them all into object definitions. 
     * 
     * @param endPoints visualisation endpoints
     * 
     * @returns promise object
     */
    public loadDefinitions(endPoints: string[]) {
        this.endPoints = endPoints;
        let promises = [];

        endPoints.forEach(endPoint => {
            let visFile = (endPoint.endsWith("/")) ? (endPoint + "visualisation.json") : (endPoint + "/visualisation.json");
            promises.push(Manager.DATA_STORE.loadDataGroups(visFile));
        });

        return Promise.all(promises);
    }

    /**
     * 
     */
    public loadImagesAndLinks() {
        let promises = [];

        this.endPoints.forEach(endPoint => {
            if(Manager.PROVIDER === MapProvider.MAPBOX) {
                let iconFile = (endPoint.endsWith("/")) ? (endPoint + "icons.json") : (endPoint + "/icons.json");
                promises.push(
                    (<MapHandler_MapBox> this.mapHandler).addIcons(iconFile)
                );
            }

            let linksFile = (endPoint.endsWith("/")) ? (endPoint + "links.json") : (endPoint + "/links.json");
            promises.push(this.panelHandler.addLinks(linksFile));
        });
        return Promise.all(promises);
    }

    /**
     * Returns the depth-first, leaf data group to be used as the 
     * default source of plotted data.
     */
    private getDefaultGroup(): DataGroup {
        if(Manager.DATA_STORE.dataGroups.length === 0) {
            throw new Error("No data has been loaded!");
        }
        let firstRoot = Manager.DATA_STORE.dataGroups[0];
        return DataUtils.getDefaultGroup(firstRoot);
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
     * 
     */
    public async featureSelectChange(select: HTMLInputElement) {
        if(window.selectFeatures !== null && window.selectFeatures !== undefined) {
            let feature = window.selectFeatures[select.value];
            this.showFeature(feature);
        } else {
            console.error("Could not find feature cached with key: " + select.value);
        }
        
        // Clear cache
        window.selectFeatures = {};
    }

    public showFeature(feature: Object) {
        // Title
        let name = feature["properties"]["name"];
        if(name === null || name === undefined) {
            name = "Feature " + feature["id"];
        }
        this.panelHandler.setTitle("<h3>" + name + "</h2");

        // Description
        let desc = feature["properties"]["description"];
        if(desc === null && feature["properties"]["desc"]) {
            desc = feature["properties"]["desc"];
        }
        if(desc !== null && desc !== undefined) {
            this.panelHandler.setContent("<div class='description'>" + desc + "</div>");
        } else {
            this.panelHandler.setContent("");
        }

        // Metadata
        let metadataURL = feature["properties"]["metadataURL"];
        if(metadataURL !== null && metadataURL !== undefined) {
            this.panelHandler.addMetadata(metadataURL);
        }

        // Timeseries
        let timeseriesURL = feature["properties"]["timeseriesURL"];
        if(timeseriesURL !== null && timeseriesURL !== undefined) {
            console.log("timeseriesURL: " + timeseriesURL);
            this.panelHandler.addTimeseries(timeseriesURL);
        }

        // Simulate click on meta button
        let metaTreeButton = document.getElementById("treeButton");
        if(metaTreeButton !== null) metaTreeButton.click();

        // Update footer
        document.getElementById("footerContainer").innerHTML = `
            <div id="returnContainer">
                <a href="#" onclick="manager.goToDefaultPanel()">&lt; Return</a>
            </div>
        `;
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

    public updateTimeseries(setName) {
        this.panelHandler.updateTimeseries(setName);
    }

    /**
	 * Shows debugging info, like mouse position.
	 */
	public showInfoPanel() {
		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "block !important";

		let self = this;
		MapHandler.MAP.on("mousemove", function(event) {
			self.updateInfoPanel(event);
		});
	}

	/**
	 * Update developer info panel.
	 */
	private updateInfoPanel(event) {
        if(this.editingCoords) return;

		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "block";

        let lng, lat;
        if(event === null || event === undefined) {
            lng = document.getElementById("lngCell").innerHTML;
		    lat = document.getElementById("latCell").innerHTML;
        } else {
            lng = event.lngLat.lng.toFixed(5);
		    lat = event.lngLat.lat.toFixed(5);
        }
		

        let coordsContainer = document.getElementById("coordsContainer");
		coordsContainer.innerHTML = `
			<table class="infoContainer" style="width: 100%; table-layout: fixed;">
				<tr>
                    <td width="60%">Longitude (at cursor):</td>
					<td width="40%" id="lngCell">` + lng + `</td>
				</tr>
				<tr>
                    <td width="60%">Latitude (at cursor):</td>
					<td width="40%" id="latCell">` + lat + `</td>
				</tr>
			</table>
		`;
	}

    public editInfoPanel() {
        let lng = document.getElementById("lngCell").innerHTML;
		let lat = document.getElementById("latCell").innerHTML;

        let coordsContainer = document.getElementById("coordsContainer");
		coordsContainer.innerHTML = `
			<table class="infoContainer" style="margin-top: 10px; width: 100%; table-layout: fixed;">
				<tr>
					<td width="50%">Map longitude:</td>
					<td width="50%"><input id="lngCell" type="number" style="width: 100%;" value="` + lng + `"></input></td>
				</tr>
				<tr>
					<td width="50%">Map latitude:</td>
					<td width="50%"><input id="latCell" type="number" style="width: 100%;" value="` + lat + `"></input></td>
				</tr>
                <tr>
                    <td width="50%"></td>
					<td width="50%"><button style="width: 100%;" onclick="manager.moveMap()">Apply</button></td>
				</tr>
			</table>
		`;
        this.editingCoords = true;
    }

    public moveMap() {
        let lng = (document.getElementById("lngCell") as HTMLInputElement).value;
		let lat = (document.getElementById("latCell") as HTMLInputElement).value;

        let target = [lng, lat];
        console.log(target);

        MapHandler.MAP.jumpTo({
            center: target
        });

        this.editingCoords = false;
        this.updateInfoPanel(null);
    }

}