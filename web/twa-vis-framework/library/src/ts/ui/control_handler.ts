/**
 * Handles building and interactions for the Camera, Terrain, and Layer Tree controls (i.e.
 * the ones on the left).
 * 
 * TODO: All tree functionality is handled manually within this file, would be good to clean it up
 * (or replace it with some external tree library) in future.
 */
class ControlHandler {

    /**
     * True if currently editing map coords.
     */
    public editingCoords: boolean = false;

    /**
     * Handles the group/layer tree.
     */
	private treeHandler: TreeHandler;

    /**
     * Constructor
     */
    constructor() {
        window.terrain = "light";
        this.treeHandler = new TreeHandler();

        // Remove deprecated Cesium popup element
        if(Manager.PROVIDER === MapProvider.CESIUM) {
            let oldElement = document.getElementById("cesiumMetaBox");
            if(oldElement != null) document.body.removeChild(oldElement);
        }
    }

    /**
     * Setup functionality to collapse control blocks.
     */
    private setupCollapses() {
        let container = document.getElementById("controlContainer");
        let titles = container.querySelectorAll(".controlTitle p");

        titles.forEach(title => {

            title.addEventListener("click", function() {
                let block = title.parentElement.parentElement;
                if(block.classList.contains("collapsed")) {
                    block.classList.remove("collapsed");
                } else {
                    block.classList.add("collapsed");
                }
            });
        });
    }

    /**
     * Adds a callback that will fire with two arrays of layerIDs (visible, hidden)
     * when any selection state in the layer tree changes.
     * 
     * @param treeSelectionCallback callback function.
     */
      public addTreeSelectionCallback(treeSelectionCallback) {
        this.treeHandler.addTreeSelectionCallback(treeSelectionCallback);
    }

	/**
	 * Generate the HTML for the control elements and add them to the 
	 * "controlsContainer" element.
	 * 
	 * @param {string} treeFile JSON file defining layer tree
	 */
	showControls() {
        this.setupCollapses();
        this.generateImageryOptions();
	}

	/**
	 * Rebuild the tree control.
	 */
	rebuildTree(dataStore: DataStore) {
        this.treeHandler.rebuild(dataStore);
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
	public updateInfoPanel(event) {
        if(this.editingCoords) return;

		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "block";

        let lng, lat;
        if(event === null || event === undefined) {
            lng = document.getElementById("lngCell").innerHTML;
		    lat = document.getElementById("latCell").innerHTML;
        } else if(event.lngLat !== null) {
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

    /**
     * Provides controls to change the map coordinates.
     */
    public editInfoPanel() {
        let lng = document.getElementById("lngCell").innerHTML;
		let lat = document.getElementById("latCell").innerHTML;

        let coordsContainer = document.getElementById("coordsContainer");
		coordsContainer.innerHTML = `
			<table class="infoContainer" style="pafdding-top: 5px; width: 100%; table-layout: fixed;">
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

    /**
     * Generates controls to change the underlying map imagery based on the "imagery"
     * property of the current global settings.
     */
    public generateImageryOptions() {
        let container = document.getElementById("imageryContainer");
        let imagerySettings = Manager.SETTINGS.getSetting("imagery");

        let defaultImagery = imagerySettings["default"];

        Object.keys(imagerySettings).forEach(function(key) {
            if(key !== "default") {
                let buttonHTML = `<input type="radio" name="terrain" id="` + key + `"`;

                switch(Manager.PROVIDER) {
                    case MapProvider.MAPBOX:
                        buttonHTML += ` onclick="MapboxUtils.changeTerrain(this.id)"`;
                    break;

                    case MapProvider.CESIUM:
                        buttonHTML += ` onclick="CesiumUtils.changeTerrain(this.id)"`;
                    break;
                }
                buttonHTML += (key === defaultImagery) ? " checked>" : ">";

                let labelHTML = `<label for="` + key + `">` + key + `</label><br/>`;
                container.innerHTML += buttonHTML + labelHTML;
            }
        })
    }

}
// End of class.