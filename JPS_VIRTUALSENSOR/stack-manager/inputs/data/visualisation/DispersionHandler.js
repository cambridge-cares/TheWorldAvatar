class DispersionHandler {
    constructor(agentBaseUrl, manager) {
        this.agentBaseUrl = agentBaseUrl;
        this.manager = manager;
    }

    setDispersions(dispersionHandler) {
        return new Promise(function (resolve) {
            let setDispersionFunction = (function (dispersionsJson) {
                dispersionHandler.dispersions = dispersionsJson;
                dispersionHandler.selectedSimulation = Object.keys(dispersionHandler.dispersions)[0];
            });
            dispersionHandler.queryForDispersions(dispersionHandler, setDispersionFunction).then(() => resolve());
        });
    }

    queryForDispersions(dispersionHandler, callback) {
        let url = dispersionHandler.agentBaseUrl;
        url += "/dispersion-interactor/GetDispersionSimulations";
        return $.getJSON(url, function (rawJSON) {
            callback(rawJSON);
        });
    }

    changeOther(show) {
        let controls = document.getElementById("controlsContainer");
        controls.style.visibility = (show) ? "visible" : "hidden";

        let slideButton = document.getElementById("slideButtonContainer");
        slideButton.style.visibility = (show) ? "visible" : "hidden";

        let expandButton = document.getElementById("expandButtonContainer");
        expandButton.style.visibility = (show) ? "visible" : "hidden";
    }

    buildDropdown(dispersionHandler) {
        return new Promise(function (resolve) {
            dispersionHandler.buildSimulationDropdown(dispersionHandler.dispersions);
            dispersionHandler.buildPollutantDropdown(dispersionHandler.dispersions[dispersionHandler.selectedSimulation]);
            dispersionHandler.buildTimestepDropdown(dispersionHandler.dispersions[dispersionHandler.selectedSimulation]);
            dispersionHandler.buildHeightDropdown(dispersionHandler.dispersions[dispersionHandler.selectedSimulation]);

            if ((document.getElementById("Simulation") != null) && (document.getElementById("Pollutant") != null)
                && (document.getElementById("Timestep") != null) && (document.getElementById("Height") != null)) {
                resolve();
            }
        });
    }

    buildSimulationDropdown(dispersions) {
        let selectionsContainer = document.getElementById("selectionsContainer");
        selectionsContainer.innerHTML = "";

        let elementId = "Simulation";
        let element = document.createElement("div");
        element.id = "selectContainer";

        let elementLabel = document.createElement("label");
        elementLabel.setAttribute("for", elementId);
        elementLabel.innerHTML = "Simulation:";

        let elementSelect = document.createElement("select");
        elementSelect.id = elementId;
        elementSelect.setAttribute("onchange", "dispersionHandler.onSimulationChange(this.value)");

        element.appendChild(elementLabel);
        element.appendChild(elementSelect);

        for (let i in Object.keys(dispersions)) {
            let dispersionId = Object.keys(dispersions)[i];
            let option = document.createElement("option");
            option.setAttribute("value", dispersionId);
            option.innerHTML = dispersionId;
            elementSelect.appendChild(option);

            if (i == 0) {
                this.selectedSimulation = Object.keys(dispersions)[i];
            }
        }
        selectionsContainer.appendChild(element);
    }

    buildPollutantDropdown(dispersion) {
        let selectionsContainer = document.getElementById("selectionsContainer");

        let pollutantElementId = "Pollutant";

        let pollutantElementSelect;
        if (document.getElementById(pollutantElementId) == null) {
            let pollutantElement = document.createElement("div");
            pollutantElement.id = "selectContainer";

            let pollutantElementLabel = document.createElement("label");
            pollutantElementLabel.setAttribute("for", pollutantElementId);
            pollutantElementLabel.innerHTML = "Pollutant:";

            pollutantElementSelect = document.createElement("select");
            pollutantElementSelect.id = pollutantElementId;
            pollutantElementSelect.setAttribute("onchange", "dispersionHandler.onPollutantChange(this.value)");

            pollutantElement.appendChild(pollutantElementLabel);
            pollutantElement.appendChild(pollutantElementSelect);

            selectionsContainer.appendChild(pollutantElement);
        } else {
            pollutantElementSelect = document.getElementById(pollutantElementId);
            pollutantElementSelect.innerHTML = "";
        }

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
    }

    buildTimestepDropdown(dispersion) {
        let selectionsContainer = document.getElementById("selectionsContainer");
        let timestepElementId = "Timestep";

        let timestepElementSelect;

        if (document.getElementById(timestepElementId) == null) {
            let timestepElement = document.createElement("div");
            timestepElement.id = "selectContainer";

            let timestepElementLabel = document.createElement("label");
            timestepElementLabel.setAttribute("for", timestepElementId);
            timestepElementLabel.innerHTML = "Timestep:";

            timestepElementSelect = document.createElement("select");
            timestepElementSelect.id = timestepElementId;
            timestepElementSelect.setAttribute("onchange", "dispersionHandler.onTimestepChange(this.value)");

            timestepElement.appendChild(timestepElementLabel);
            timestepElement.appendChild(timestepElementSelect);

            selectionsContainer.appendChild(timestepElement);
        } else {
            timestepElementSelect = document.getElementById(timestepElementId);
            timestepElementSelect.innerHTML = "";
        }

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
    }

    buildHeightDropdown(dispersion) {
        let selectionsContainer = document.getElementById("selectionsContainer");

        let heightElementId = "Height";

        let heightElementSelect;
        if (document.getElementById(heightElementId) == null) {
            let heightElement = document.createElement("div");
            heightElement.id = "selectContainer";

            let heightElementLabel = document.createElement("label");
            heightElementLabel.setAttribute("for", heightElementId);
            heightElementLabel.innerHTML = "Height:";

            heightElementSelect = document.createElement("select");
            heightElementSelect.id = heightElementId;
            heightElementSelect.setAttribute("onchange", "dispersionHandler.onHeightChange(this.value)");

            heightElement.appendChild(heightElementLabel);
            heightElement.appendChild(heightElementSelect);

            selectionsContainer.appendChild(heightElement);
        } else {
            heightElementSelect = document.getElementById(heightElementId);
            heightElementSelect.innerHTML = "";
        }

        let heights = Object.keys(dispersion['z']).sort(function (a, b) { return a - b });
        for (let i in heights) {
            let height = heights[i];
            let heightOption = document.createElement("option");
            heightOption.setAttribute("value", height);
            heightOption.innerHTML = height + " m";
            heightElementSelect.appendChild(heightOption);

            if (i == 0) {
                this.selectedHeight = height;
            }
        }
    }

    onPollutantChange(pollutant) {
        this.selectedPollutant = pollutant;
        this.plotData();
    }

    onTimestepChange(timestep) {
        this.selectedTimestep = timestep;
        this.plotData();
    }

    onHeightChange(height) {
        this.selectedHeight = height;
        this.plotData();
    }

    onSimulationChange(dispersion) {
        this.selectedSimulation = dispersion;
        this.buildPollutantDropdown(this.dispersions[this.selectedSimulation]);
        this.buildTimestepDropdown(this.dispersions[this.selectedSimulation]);
        this.buildHeightDropdown(this.dispersions[this.selectedSimulation]);
        this.plotData();
        MapHandler.MAP.jumpTo({ center: this.dispersions[this.selectedSimulation].centroid });
    }

    plotData() {
        this.updateVirtualSensors(this.dispersions[this.selectedSimulation].derivationIri);
        let dataJsonUrl = this.agentBaseUrl;
        dataJsonUrl += '/dispersion-interactor/GetDataJson?';

        let params = {
            pollutant: this.selectedPollutant,
            timestep: this.selectedTimestep,
            scopeLabel: this.selectedSimulation,
            derivationIri: this.dispersions[this.selectedSimulation].derivationIri,
            pollutantLabel: this.dispersions[this.selectedSimulation].pollutants[this.selectedPollutant],
            weatherStation: JSON.stringify(this.dispersions[this.selectedSimulation].weatherStation),
            z: this.selectedHeight
        };

        let searchParams = new URLSearchParams(params);
        dataJsonUrl += searchParams;

        let plotFunction = (function () {
            this.manager.plotData();
        }).bind(this);

        this.manager.loadDefinitionsFromURL(dataJsonUrl).then(() =>
            plotFunction()
        );
        this.addColourBar();
    }

    addColourBar() {
        let colourBarElement;
        if (document.getElementById("colorbar-legend-container") != null) {
            colourBarElement = document.getElementById("colorbar-legend-container");
        } else {
            let mapContainer = document.getElementById("map");
            colourBarElement = document.createElement("div");
            colourBarElement.id = "colorbar-legend-container";
            mapContainer.appendChild(colourBarElement);
        }

        let params = {
            pollutant: this.selectedPollutant,
            timestep: this.selectedTimestep,
            derivationIri: this.dispersions[this.selectedSimulation].derivationIri,
            zIri: this.dispersions[this.selectedSimulation].z[this.selectedHeight]
        };

        let colourBarUrl = this.agentBaseUrl;
        colourBarUrl += '/dispersion-interactor/GetColourBar?';

        let searchParams = new URLSearchParams(params);
        colourBarUrl += searchParams;

        colourBarElement.setAttribute("style", "background-image: url(" + colourBarUrl + ")");
    }

    createVirtualSensor(lat, lng) {
        let url = this.agentBaseUrl;
        url += '/dispersion-interactor/CreateVirtualSensor?';

        let params = {
            lat: lat,
            lng: lng,
        };

        let searchParams = new URLSearchParams(params);

        Object.keys(this.dispersions[this.selectedSimulation].pollutants).forEach(pollutant => searchParams.append("pollutant", pollutant));
        url += searchParams;

        return $.post(url);
    }

    updateVirtualSensors(derivation) {
        let url = this.agentBaseUrl;
        url += '/dispersion-interactor/UpdateVirtualSensors?';

        let params = {
            derivation: derivation
        };

        let searchParams = new URLSearchParams(params);
        url += searchParams;

        $.post(url);
    }
}