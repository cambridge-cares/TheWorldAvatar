/**
 * Handles the side panel
 */
class PanelHandler {

    /**
     * Default state of general tab
     */
    _defaultHTML: string;

    /**
     * Handles plotting time series data.
     */
    timeseriesHandler: TimeseriesHandler;

    /**
     * Constructor
     */
    constructor() {
        this.timeseriesHandler = new TimeseriesHandler();
    }

    /**
	 * Toggles the visibility of the legend element.
	 * 
	 * @param {boolean} visible 
	 */
	public toggleLegend(visible) {
		document.getElementById("legendContainer").style.display = (visible) ? "block" : "none";
	}

    /**
	 * Sets the title of the side panel.
	 * 
	 * @param {Stirng} title desired title HTML
	 */
	public setTitle(title) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("titleContainer").innerHTML = title;
	}

	/** 
	 * Set the main content of the side panel.
	 * 
	 * @param {String} contentHTML HTML to add.
	 */
    public setContent(contentHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("contentContainer").innerHTML = contentHTML;
		
		var sidePanel = document.getElementById("sidePanel");
		if(sidePanel.classList.contains("large")) {
			document.getElementById("controlsParent").style.visibility = "hidden";
		}
	}

    /**
     * 
     */
    public appendContent(contentHTML) {
        document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("contentContainer").innerHTML += contentHTML;
    }

	/**
	 * Override the legend content.
	 * 
	 * @param {String} legendHTML HTML to add. 
	 */
    public setLegend(legendHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("sidePanelLegend").innerHTML = legendHTML;
	}

    /**
	 * Set the footer content.
	 * 
	 * @param {String} footerHTML HTML to add. 
	 */
	public setFooter(footerHTML) {
		document.getElementById("sidePanel").style.visibility = "visible";
		document.getElementById("footerContainer").innerHTML = footerHTML;
	}

    /**
	 * Store the current state of the side panel as it's default
	 */
	public storeDefault() {
		this._defaultHTML = document.getElementById("sidePanelGeneral").innerHTML;
	}

	/**
	 * Return the contents to their default state.
	 */
	public returnToDefault() {
		if(this._defaultHTML != null) {
			document.getElementById("sidePanelGeneral").innerHTML = this._defaultHTML;
		}
        document.getElementById("returnContainer").style.display = "none";
        window.currentFeature = null;

        // Simulate click on general tab
        // @ts-ignore
        $("#sidePanelInner").tabs("option", "active", 0);
	}

    /**
	 * Changes the mode of the side panel.
	 */
	public toggleMode() {
		var sidePanel = document.getElementById("sidePanel");
		var leftButton = document.getElementById("slideButton");
		var rightButton = document.getElementById("expandButton");

		if(sidePanel.classList.contains("small")) {
			// Make large
			sidePanel.classList.replace("small", "large");
            rightButton.classList.replace("fa-expand", "fa-compress");

			document.getElementById("map").style.width = "100%";
			document.getElementById("controlsContainer").style.visibility = "hidden";

			leftButton.style.visibility = "hidden";

			// Stop keyboard events
			// MapHandler.MAP["keyboard"].disable();
			// MapHandler.MAP.resize();

		} else if(sidePanel.classList.contains("large")) {
			// Make small
			sidePanel.classList.replace("large", "small");
            rightButton.classList.replace("fa-compress", "fa-expand");

			document.getElementById("map").style.width = "calc(100% - 500px)";
			document.getElementById("controlsContainer").style.visibility = "visible";

			leftButton.style.visibility = "visible";

			// Allow keyboard events
			// MapHandler.MAP["keyboard"].enable();
			// MapHandler.MAP.resize();
		}
	}

    /**
	 * Updates the visibility of the side panel.
	 */
	public toggleExpansion() {
		var sidePanel = document.getElementById("sidePanel");
		var sidePanelInner = document.getElementById("sidePanelInner");
		var leftButton = document.getElementById("slideButton");
        var rightButton = document.getElementById("expandButton");
        var finderContainer = document.getElementById("finderContainer");

		if(sidePanel.classList.contains("small")) {

			if(sidePanel.classList.contains("collapsed")) {
				// Expand
				sidePanel.classList.replace("collapsed", "expanded");
                leftButton.classList.replace("fa-chevron-left", "fa-chevron-right");

				document.getElementById("map").style.width = "calc(100% - 500px)";

				rightButton.style.visibility = "visible";
				sidePanelInner.style.visibility = "visible";

                finderContainer.classList.replace("collapsed", "expanded");
				
			} else if(sidePanel.classList.contains("expanded")) {
				// Collapse
				sidePanel.classList.replace("expanded", "collapsed");
                leftButton.classList.replace("fa-chevron-right", "fa-chevron-left");

				document.getElementById("map").style.width = "calc(100% - 28px)";

				rightButton.style.visibility = "hidden";
				sidePanelInner.style.visibility = "hidden";

                finderContainer.classList.replace("expanded", "collapsed");
			}
		} 

		MapHandler.MAP.resize();
	}


    /**
     * Request and display metadata and timeseries for the input feature IRI.
     * 
     * @param iri 
     * @param stackURL 
     * @param endpoint 
     * @returns 
     */
    public addSupportingData(feature, properties) {
        // Get required details
        let iri = properties["iri"];
        let stack = Manager.findStack(feature, properties);

        if(iri == null || stack == null) {
            console.warn("Feature is missing required information to get metadata/timeseries, will show in-model content instead...");

            // Render metadata tree
            this.prepareMetaContainers(true, false);
            document.getElementById("metaTreeContainer").innerHTML = "";

            // @ts-ignore
            let metaTree = JsonView.renderJSON(properties, document.getElementById("metaTreeContainer"));
            // @ts-ignore
            JsonView.expandChildren(metaTree);
            // @ts-ignore
            JsonView.selectiveCollapse(metaTree);
            return;
        }

        // Proceed to contact agent for metadata and timeseries
        this.prepareMetaContainers(true, true);

        // Build the request to the FeatureInfoAgent
        let agentURL = stack + "/feature-info-agent/get";
        let params = { "iri": iri };

        let self = this;
        var promise = $.getJSON(agentURL, params, function(json) {
            // Get results
            let meta = json["meta"];
            let time = json["time"];

            // Render metadata tree
            document.getElementById("metaTreeContainer").innerHTML = "";

            if(meta !== null && meta !== undefined) {
                // @ts-ignore
                let metaTree = JsonView.renderJSON(meta, document.getElementById("metaTreeContainer"));
                // @ts-ignore
                JsonView.expandChildren(metaTree);
                // @ts-ignore
                JsonView.selectiveCollapse(metaTree);
            }

            // Render timeseries
            document.getElementById("metaTimeContainer").innerHTML = "";

            if(time !== null && time !== undefined) {
                // Plot data
                self.timeseriesHandler.parseData(time);
                self.timeseriesHandler.showData("metaTimeContainer");

                // Auto-select the first option in the dropdown
                let select = document.getElementById("time-series-select") as HTMLInputElement;
                select.onchange(null);
            }
        });
        return promise;
    }

    public updateTimeseries(setName) {
        this.timeseriesHandler.update(setName);
    }


    private prepareMetaContainers(addMeta: boolean, addTime: boolean) {
        let metaTabs = document.getElementById("metaTabs") as HTMLInputElement;
        if(metaTabs === null || metaTabs === undefined) {
            this.appendContent("<div id='metaTabs'></div>");
        }

        let metaContainer = document.getElementById("metaContainer") as HTMLInputElement;
        if(metaContainer === null || metaContainer === undefined) {
            this.appendContent("<div id='metaContainer'></div>");
        }

        if(addMeta) {
            if(document.getElementById("treeButton") === null) {
                document.getElementById("metaTabs").innerHTML += `
                    <button id="treeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTreeContainer')">Metadata</button>
                `;
            }
            if(document.getElementById("metaTreeContainer") === null) {
                document.getElementById("metaContainer").innerHTML += "<div id='metaTreeContainer' class='tabcontent'></div>"
            }
        }

        if(addTime) {
            if(document.getElementById("timeButton") === null) {
                document.getElementById("metaTabs").innerHTML += `
                    <button id="timeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTimeContainer')">Time Series</button>
                `;
            }
            if(document.getElementById("metaTimeContainer") === null) {
                document.getElementById("metaContainer").innerHTML += "<div id='metaTimeContainer' style='display: none;' class='tabcontent'></div>"
            }
        }

        let footerContent = document.getElementById("footerContainer");
        if(footerContent !== null) footerContent.style.display = "none";
    }

    public addLinks(linksFile: string) {
        return $.getJSON(linksFile, function(json) {
            return json;
        })
        .fail(() => {
            console.warn("Could not read links.json, skipping.");
            let linksTab = document.querySelector("li[aria-controls=\"sidePanelLinks\"]") as HTMLElement;
            if(linksTab !== null) linksTab.style.display = "none";
        })
        .done((json) => {
            if(json === null || !json["links"]) {
                let linksTab = document.querySelector("li[aria-controls=\"sidePanelLinks\"]") as HTMLElement;
                if(linksTab !== null) linksTab.style.display = "none";
                return;
            }

            let container = document.getElementById("sidePanelLinks");
            if(container === null) return;

            container.innerHTML += "<br/><br/>";
            
            if(json["intro"]) {
                container.innerHTML += "<p>" + json["intro"] + "</p>"
            }

            if(json["links"]) {
                container.innerHTML += "<ul>";

                for(let i = 0; i < json["links"].length; i++) {
                    let entry = json["links"][i];
                    container.innerHTML += "<li><a href='" + entry["url"] + "' target='_blank'>" + entry["text"] + "</a></li>";
                }
                container.innerHTML += "</ul>";
            }
        });
    }
}
