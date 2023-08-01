class DispersionHandler {
    constructor(agentBaseUrl, manager) {
        this.agentBaseUrl = agentBaseUrl;
        this.manager = manager;
        let scenarioButton = document.getElementById("scenarioChangeContainer");
        scenarioButton.style.display = "block";
    }

    showSelector() {
        let container = document.getElementById("scenario-container");

        if (container == null) {
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
                            <p style="color: grey; font-style: italic;">Loading scenario details, please wait...</p>
                        </div>
                    </div>
                </div>
            `;
            document.body.appendChild(container);

            let setDispersionFunction = (function (dispersionsJson) {
                this.dispersions = dispersionsJson;
                this.buildComponents();
            }).bind(this);

            this.queryForDispersions(this, setDispersionFunction);
        }

        // Hide the close button if there's no scenario already selected
        if (this.selectedScenario == null) {
            let closeButton = document.getElementById("scenario-close");
            closeButton.style.display = "none";
        }

        // Update the width of the container
        let map = document.getElementById("map");
        container.style.width = map.style.width;

        this.changeOther(false);

        container.style.display = "block";
    }

    queryForDispersions(dispersionHandler, callback) {
        let url = dispersionHandler.agentBaseUrl;
        url += "/dispersion-interactor/GetDispersionSimulations";
        $.getJSON(url, function (rawJSON) {
            callback(rawJSON);
        });
    }

    buildComponents() {
        let container = document.getElementById("scenario-inner");
        container.innerHTML = "";

        for (let key in this.dispersions) {
            // Create element
            let element = document.createElement("div");
            element.id = key;
            element.classList.add("scenario-element");

            // Set contents
            element.innerHTML = `
                <b> ` + key + `</b><br/>
                <button type="button" class="scenario-button" id="` + element.id + `" onclick="dispersionHandler.selectScenario(id)">View</button>
            `;

            // Add to container
            container.appendChild(element);
        }
    }

    changeOther(show) {
        let controls = document.getElementById("controlsContainer");
        controls.style.visibility = (show) ? "visible" : "hidden";

        let slideButton = document.getElementById("slideButtonContainer");
        slideButton.style.visibility = (show) ? "visible" : "hidden";

        let expandButton = document.getElementById("expandButtonContainer");
        expandButton.style.visibility = (show) ? "visible" : "hidden";
    }

    selectScenario(dispersionId) {
        this.selectedScenario = dispersionId;

        let container = document.getElementById("scenario-container");
        if (container != null) container.style.display = "none";

        this.buildDropdown(this.dispersions[dispersionId]);

        this.changeOther(true);
    }

    buildDropdown(dispersion) {
        this.buildPollutantDropdown(dispersion);
        this.buildTimestepDropdown(dispersion);
    }

    buildPollutantDropdown(dispersion) {
        let selectionsContainer = document.getElementById("selectionsContainer");
        selectionsContainer.innerHTML = "";

        let pollutantElementId = "Pollutant";
        let pollutantElement = document.createElement("div");
        pollutantElement.id = "selectContainer";

        let pollutantElementLabel = document.createElement("label");
        pollutantElementLabel.setAttribute("for", pollutantElementId);
        pollutantElementLabel.innerHTML = "Pollutant:";

        let pollutantElementSelect = document.createElement("select");
        pollutantElementSelect.id = pollutantElementId;
        pollutantElementSelect.setAttribute("onchange", "dispersionHandler.onPollutantChange(this.value)");

        pollutantElement.appendChild(pollutantElementLabel);
        pollutantElement.appendChild(pollutantElementSelect);

        let pollutants = Object.keys(dispersion['pollutants']);
        for (let i in pollutants) {
            let pollutant = pollutants[i];
            let pollutantOption = document.createElement("option");
            pollutantOption.setAttribute("value", pollutant);
            pollutantOption.innerHTML = dispersion['pollutants'][pollutant];
            pollutantElementSelect.appendChild(pollutantOption);

            if (i == 0) {
                this.selectedPollutant = pollutant;
            }
        }
        selectionsContainer.appendChild(pollutantElement);
    }

    buildTimestepDropdown(dispersion) {
        let timestepElementId = "Timestep";
        let timestepElement = document.createElement("div");
        timestepElement.id = "selectContainer";

        let timestepElementLabel = document.createElement("label");
        timestepElementLabel.setAttribute("for", timestepElementId);
        timestepElementLabel.innerHTML = "Timestep:";

        let timestepElementSelect = document.createElement("select");
        timestepElementSelect.id = timestepElementId;
        timestepElementSelect.setAttribute("onchange", "dispersionHandler.onTimestepChange(this.value)");

        timestepElement.appendChild(timestepElementLabel);
        timestepElement.appendChild(timestepElementSelect);

        let timesteps = dispersion['time'];
        for (let i in timesteps) {
            let timestep = timesteps[i];
            let timestepOption = document.createElement("option");
            timestepOption.setAttribute("value", timestep);
            timestepOption.innerHTML = timestep;
            timestepElementSelect.appendChild(timestepOption);

            if (i == 0) {
                this.selectedTimestep = timestep;
            }
        }
        selectionsContainer.appendChild(timestepElement);
    }

    onPollutantChange(pollutant) {
        this.selectedPollutant = pollutant;
    }

    onTimestepChange(timestep) {
        this.selectedTimestep = timestep;
    }


}