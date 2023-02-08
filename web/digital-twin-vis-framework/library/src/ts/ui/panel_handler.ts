/**
 * Handles the side panel
 */
class PanelHandler {

    /**
     * Default state of general tab
     */
    _defaultHTML: string;

    /**
     * 
     */
    manager: Manager;

    /**
     * Handles plotting time series data.
     */
    timeseriesHandler: TimeseriesHandler;

    /**
     * Constructor
     */
    constructor(manager) {
        this.manager = manager;
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
        var attributions = document.getElementById("attributionContainer");

		if(sidePanel.classList.contains("small")) {
			// Make large
			sidePanel.classList.replace("small", "large");
            rightButton.classList.replace("fa-expand", "fa-compress");

			document.getElementById("map").style.width = "100%";
			document.getElementById("controlsContainer").style.visibility = "hidden";
			leftButton.style.visibility = "hidden";
            
            if(attributions != null) attributions.style.display = "none";

		} else if(sidePanel.classList.contains("large")) {
			// Make small
			sidePanel.classList.replace("large", "small");
            rightButton.classList.replace("fa-compress", "fa-expand");

			document.getElementById("map").style.width = "calc(100% - 500px)";
			document.getElementById("controlsContainer").style.visibility = "visible";
			leftButton.style.visibility = "visible";

            if(Manager.SETTINGS.getSetting("attribution") != null && attributions != null) {
                attributions.style.display = "block";
            }
		}

        MapHandler.MAP.resize();
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

                if(finderContainer != null) finderContainer.style.width = "calc(100% - 540px)";
				
			} else if(sidePanel.classList.contains("expanded")) {
				// Collapse
				sidePanel.classList.replace("expanded", "collapsed");
                leftButton.classList.replace("fa-chevron-right", "fa-chevron-left");

				document.getElementById("map").style.width = "calc(100% - 28px)";

				rightButton.style.visibility = "hidden";
				sidePanelInner.style.visibility = "hidden";

                if(finderContainer != null) finderContainer.style.width = "calc(100% - 80px)";
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
        properties = filterNulls(properties);

        // Get required details
        let iri = properties["iri"];
        let endpoint = properties["endpoint"];

        let stack = Manager.findStack(feature, properties);
        console.log("Attempting to contact agent with stack at '" + stack + "'...");
        console.log("   ...will submit IRI for query '" + iri + "'");

        if(iri == null || stack == null) {
            console.warn("Feature is missing required information to get metadata/timeseries, will show any in-model content instead...");
            this.showBuiltInData(properties);
            return;
        }

        // Proceed to contact agent for metadata and timeseries
        this.prepareMetaContainers(true, true);
        document.getElementById("metaTreeContainer").innerHTML = "<i>Retrieving data...</i>";
        
        // Build the request to the FeatureInfoAgent
        let agentURL = stack + "/feature-info-agent/get";
        let params = { 
            "iri": iri,
            "endpoint": endpoint
        };

        let self = this;
        var promise = $.getJSON(agentURL, params, function(rawJSON) {
            if(rawJSON === null || rawJSON === undefined) {
                self.showBuiltInData(properties);
                return;
            }
            if(Array.isArray(rawJSON) && rawJSON.length == 0) {
                self.showBuiltInData(properties);
                return;
            }
            if(Object.keys(rawJSON).length == 0) {
                self.showBuiltInData(properties);
                return;
            }

            // Get results
            let meta = rawJSON["meta"];
            let time = rawJSON["time"];

            if (meta != null) console.log("Got a meta object!");
            if (time != null) console.log("Got a time object!");

            // Render metadata tree
            document.getElementById("metaTreeContainer").innerHTML = "";

            if(meta != null) {
                // Formatting
                meta = JSONFormatter.formatJSON(meta);

                let treeContainer = document.getElementById("metaTreeContainer");
                if(treeContainer == null) console.log("TREE CONTAINER IS NULL, WHAT?!");

                if(Array.isArray(meta) && meta.length === 0) {
                    this.showBuiltInData(properties);
                } else if (typeof meta === "string" && meta === "") {
                    this.showBuiltInData(properties);
                } else {
                    // @ts-ignore
                    let metaTree = JsonView.renderJSON(meta, document.getElementById("metaTreeContainer"));
                    // @ts-ignore
                    JsonView.expandChildren(metaTree);
                    // @ts-ignore
                    JsonView.selectiveCollapse(metaTree);
                }
            } else {
                self.showBuiltInData(properties);
            }

            // Render timeseries
            document.getElementById("metaTimeContainer").innerHTML = "";

            if(time != null) {
                // Plot data
                self.timeseriesHandler.parseData(time);
                self.timeseriesHandler.showData("metaTimeContainer");

                // Auto-select the first option in the dropdown
                let select = document.getElementById("time-series-select") as HTMLInputElement;
                select.onchange(null);
            } else {
                console.warn("No 'time' node found, skipping timeseries visualisation.");
            }

            // Set visibility of UI containers
            self.prepareMetaContainers(true, time != null);

        })
        .fail(function() {
            console.warn("Could not get valid response from the agent, will show any in-model content instead...");
            self.showBuiltInData(properties);
            return;
        });
        return promise;
    }

    /**
     * Show properties from the feature as metadata rather than something returned
     * from the remote FeatureInfoAgent.
     * 
     * @param properties feature properties
     */
    public showBuiltInData(properties) {
        this.prepareMetaContainers(true, false);
        document.getElementById("metaTreeContainer").innerHTML = "";

        if(Object.keys(properties).length > 0) {
            let formattedProps = JSONFormatter.formatJSON(properties);
            // @ts-ignore
            let metaTree = JsonView.renderJSON(formattedProps, document.getElementById("metaTreeContainer"));
            // @ts-ignore
            JsonView.expandChildren(metaTree);
            // @ts-ignore
            JsonView.selectiveCollapse(metaTree);
        } else {
            document.getElementById("metaTreeContainer").innerHTML = "<i>No available data.</i>";
        }
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

        let treeButton = document.getElementById("treeButton");
        let timeButton = document.getElementById("timeButton");

        if(addMeta) {
            if(treeButton === null) {
                document.getElementById("metaTabs").innerHTML += `
                    <button id="treeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTreeContainer')">Metadata</button>
                `;
                treeButton = document.getElementById("treeButton");
            }
            if(document.getElementById("metaTreeContainer") === null) {
                document.getElementById("metaContainer").innerHTML += "<div id='metaTreeContainer' class='tabcontent'></div>"
            }
        }
        if(addTime) {
            if(timeButton === null) {
                document.getElementById("metaTabs").innerHTML += `
                    <button id="timeButton" class="tablinks" onclick="manager.openMetaTab(this.id, 'metaTimeContainer')">Time Series</button>
                `;
                timeButton = document.getElementById("timeButton");
            }
            if(document.getElementById("metaTimeContainer") === null) {
                document.getElementById("metaContainer").innerHTML += "<div id='metaTimeContainer' style='display: none;' class='tabcontent'></div>"
            }
        }

        if(treeButton != null) treeButton.style.display = (addMeta) ? "block" : "none"; 
        if(timeButton != null) timeButton.style.display = (addTime) ? "block" : "none"; 

        if(addMeta && !addTime) {
            treeButton.style.width = "100%";
            treeButton.style.borderRadius = "10px";

            this.manager.openMetaTab("treeButton", "metaTreeContainer");

        } else if (!addMeta && addTime) {
            timeButton.style.width = "100%";
            timeButton.style.borderRadius = "10px";
            
            this.manager.openMetaTab("timeButton", "metaTimeContainer");

        } else if(addMeta && addTime) {
            treeButton.style.width = "50%";
            treeButton.style.borderRadius = "10px 0 0 10px";

            this.manager.openMetaTab("treeButton", "metaTreeContainer");
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
