/**
 * Handles building and interactions for the Camera, Terrain, and Layer Tree controls (i.e.
 * the ones on the left).
 * 
 * TODO: All tree functionality is handled manually within this file, would be good to clean it up
 * (or replace it with some external tree library) in future.
 */
class ControlHandler {

    // Handles the group/layer tree
	private treeHandler;

    /**
     * 
     */
    constructor() {
        window.terrain = "light";
        this.treeHandler = new TreeHandler();
    }

    /**
     * 
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
	 * Generate the HTML for the control elements and add them to the 
	 * "controlsContainer" element.
	 * 
	 * @param {string} treeFile JSON file defining layer tree
	 */
	showControls() {
        // Load controls.html file
        //return loadHTML("./html/controls.html").then(text => {
            //document.getElementById("controlsContainer").innerHTML = text;
            this.setupCollapses();

            //Ensure selected terrain option is right
            let terrainContainer = document.getElementById("terrainContainer");
            let terrainSelect = terrainContainer.querySelector("input[id='" + window.terrain + "']") as HTMLInputElement;
            if(terrainSelect != null) terrainSelect.checked = true;
        //});
	}

	/**
	 * Shows debugging info, like mouse position.
	 */
	showDeveloperControls() {
		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "none !important";

		let self = this;
		MapHandler.MAP.on("mousemove", function(event) {
			self.updateDeveloperControls(event);
		});
	}

	/**
	 * Update developer info panel.
	 */
	private updateDeveloperControls(event) {
		let developerInfo = document.getElementById("developerContainer");
		developerInfo.style.display = "block";

		let lng = event.lngLat.lng.toFixed(5);
		let lat = event.lngLat.lat.toFixed(5);
		developerInfo.innerHTML = `
			<table width="100%">
				<tr>
					<td width="35%">Longitude:</td>
					<td width="65%">` + lng + `</td>
				</tr>
				<tr>
					<td width="35%">Latitude:</td>
					<td width="65%">` + lat + `</td>
				</tr>
			</table>
		`;
	}

	/**
	 * Rebuild the tree control.
	 */
	rebuildTree(dataStore: DataStore) {
        this.treeHandler.rebuild(dataStore);
	}

}
// End of class.