/**
 * This class handles the query to determine what scenarios are available, as well as
 * the construction of the scenario selection UI component.
 */
class ScenarioHandler {

    /**
     * Base URL of the agent to contact about scenarios.
     */
    private agentBaseURL;

    /**
     * Optional dataset identify to send to the scenario agent.
     */
    private agentDataset;

    /**
     * JSON object holding definitions of possible scenarios.
     */
    private definitions;

    /**
     * ID of currently selected scenario.
     */
    public selectedScenario;
    public scenarioName;

    /**
     * Initialise a new scenario handler.
     * 
     * @param agentBaseURL Base URL of the agent to contact about scenarios.
     * @param agentDataset Optional dataset identify to send to the scenario agent.
     */
    constructor(agentBaseURL: string, agentDataset: string) {
        this.agentBaseURL = agentBaseURL;
        this.agentDataset = agentDataset;
    }

    /**
     * Submit a HTTP request to a remote URL to determine the list of
     * all available scenarios. Expects a set-format JSON string to be
     * sent back.
     * 
     * @returns Promise object
     */
    private queryForScenarios() {
        if(this.definitions != null) return Promise.resolve();
        console.log("Querying for list of available scenarios...");

        // Build params
        let params = {
            "type": "list"
        };

        // Query for scenario details
        let self = this;
        let url = this.agentBaseURL;
        url += (url.endsWith("/")) ? "getScenarios" : "/getScenarios";
        console.log("Querying for scenarios at " + url);

        let promise = $.getJSON(url, params, function(rawJSON) {
            self.definitions = rawJSON;
        }).fail(function() {
            console.error("Could not determine what scenarios are available!");
        });

        return promise;
    }

    /**
     * Shows the scenario selector component.
     */
    public showSelector() {
        let container = document.getElementById("scenario-container");

        if(container == null) {
            // Create and add the UI element
            container = document.createElement("div");
            container.id = "scenario-container";

            container.innerHTML = `
                <div id="scenario-blocker"></div>
                <div id="scenario-popup">
                    <div id="scenario-close"><i class="fa fa-times"></i></div>
                    <div id="scenario-header">
                        <b style="font-size: 120%;">Select a scenario:</b><br/><br/>
                        <p>Please identify a scenario from the list below, then select the 'View' button to plot its data.</p>
                    </div>
                    <div id="scenario-inner">
                        <div id="scenario-loading" style="display: table;">
                            <img style="width: 200px;" src="loading.gif"></img>
                            <p style="color: grey; font-style: italic;">Loading scenario details, please wait...</p>
                        </div>
                    </div>
                </div>
            `;
            document.body.appendChild(container);

            // Send of request to get the scenario details
            let self = this;
            let promise = this.queryForScenarios() as Promise<any>;
            promise.then(() => {
                this.buildComponents();
            });
        }

        // Hide the close button if there's no scenario already selected
        if(this.selectedScenario == null) {
            let closeButton = document.getElementById("scenario-close");
            closeButton.style.display = "none";
        }

        // Update the width of the container
        let map = document.getElementById("map");
        container.style.width = map.style.width;

        // Hide other controls
        this.changeOther(false);

        // Show controls
        container.style.display = "block";
    }

    /**
     * Changes the visibility of other the other UI controls).
     * 
     * @param show boolean
     */
    private changeOther(show) {
        let controls = document.getElementById("controlsContainer");
        controls.style.visibility = (show) ? "visible" : "hidden";
        
        let attributions = document.getElementById("attributionContainer");
        attributions.style.visibility = (show) ? "visible" : "hidden";

        let slideButton = document.getElementById("slideButtonContainer");
        slideButton.style.visibility = (show) ? "visible" : "hidden";

        let expandButton = document.getElementById("expandButtonContainer");
        expandButton.style.visibility = (show) ? "visible" : "hidden";
    }

    /**
     * Build the individual UI component for each scenario and add
     * them to their container.
     */
    private buildComponents() {
        let container = document.getElementById("scenario-inner");
        container.innerHTML = "";

        for(let key in this.definitions) {
            let scenario = this.definitions[key];

            // Create element
            let element = document.createElement("div");
            element.id = scenario["id"];
            element.setAttribute("name", scenario["name"]);
            element.addEventListener("click", function(){
                window.manager.selectScenario(scenario["id"], scenario["name"])
            });

            element.classList.add("scenario-element");

            // Set contents
            element.innerHTML = `
                <b> ` + scenario["name"] + `</b><br/><br/>
                <p>` + scenario["description"] + `</p><br/>
                <button type="button" class="scenario-button" id="` + element.id + `" name="` + scenario["name"] + `" + onclick="manager.selectScenario(this.id, this.name)">View</button>
            `;

            // Add to container
            container.appendChild(element);
        }
    }
    
    /**
     * Sets the selected scenario ID.
     * 
     * @param scenarioID scenario ID
     * @param scenarioName user facing scenario name
     */
    public selectScenario(scenarioID, scenarioName) {
        this.selectedScenario = scenarioID;
        this.scenarioName = scenarioName;

        let container = document.getElementById("scenario-container");
        if(container != null) container.style.display = "none";
        this.changeOther(true);
    }

    /**
     * Once a scenario has been selected, this code queries it to retrieve
     * a configuration JSON equal to a data.json file.
     * 
     * @param callback to pass JSON object to.
     */
    public getConfiguration(callback) {
        if(this.selectedScenario == null) return;
        console.log("Querying for scenario configuration file...");

        // Set query params
        let params = {
            "type": "get"
        }

        let url = this.agentBaseURL;
        url += (url.endsWith("/")) ? "getDataJson/" : "/getDataJson/";
        url += this.selectedScenario;

        if(this.agentDataset != null) {
            url += "?dataset=" + this.agentDataset;
        }

        console.log("Querying for config at " + url);
        
        // Query for scenario configuration
        $.getJSON(url, params, function(rawJSON) {
            callback(rawJSON);
        }).fail(function() {
            console.error("Could not get the data configuration for the scenario!");
        });
    }

}
// End of class.