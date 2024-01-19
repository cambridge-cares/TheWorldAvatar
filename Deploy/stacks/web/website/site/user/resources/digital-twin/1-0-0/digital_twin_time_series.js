/**
 * Handles reading and plotting standard Time Series data.
 */
class DigitalTwinTimeSeries {

    // Loaded time series data
    _data = [];

    // Current data set for the selected layer, set, and location
    _selectedData;

    //
    _currentChart;

    /**
     * Reads the input data file and stores it under the input set name.
     * 
     * @param {String} layerName what mapbox layer is this data for
     * @param {String} setName name to give this data set
     * @param {String} jsonFile JSON file containing data
     */
    readFile(layerName, setName, jsonFile) {
        var that = this;

		var rawFile = new XMLHttpRequest();
		rawFile.onreadystatechange = function() {

			if (rawFile.readyState == 4 && rawFile.status == "200") {

				console.log("INFO: Reading TimeSeries data from file at: " + jsonFile);
                that.parseData(layerName, setName, JSON.parse(rawFile.responseText));
				console.log("INFO: TimeSeries data has been read.");
			} 
		}
		rawFile.open("GET", jsonFile, true);
		rawFile.send();
    }

    /**
     * Expands the raw time series JSON and stores it in a format that easier to
     * pipe into tables and charts later.
     * 
     * @param {*} setName 
     * @param {*} rawJSON 
     */
    parseData(layerName, setName, rawJSON) {
        var allNewData = [];

        // Parse raw JSON into expanded form
        for(var i = 0; i < rawJSON.length; i++) {
            var entry = rawJSON[i];

            // May have multiple data sets with differing units
            var tableNames = entry["data"];
            var tableUnits = entry["units"];

            for(var j = 0; j < tableNames.length; j++) {
                // Dependent name
                var tableName = tableNames[j];

                // Dependent unit
                var tableUnit = tableUnits[j];
                tableName += " [" + tableUnit + "]";
                
                // Get timestamps
                var tableTimes = entry["time"];
                for(var t = 0; t < tableTimes.length; t++) {
                    tableTimes[t] = tableTimes[t].replace("T", " ").replace("Z", "");
                }

                // Get correct value array
                var tableValues = entry["values"][j];

                // Store
                allNewData.push({
                    "name": tableName,
                    "id": entry["id"],
                    "unit": tableUnit,
                    "times": tableTimes,
                    "values": tableValues
                });
            }            
        }

        // Store 
        if(!this._data[layerName]) {
            this._data[layerName] = {};
        } 
        this._data[layerName][setName] = allNewData;
    }

    /**
     * 
     * @param {String} dataSetName name of data set to look within.
     * @param {Integer} locationID value of the 'id' field within the JSON
     * 
     * @returns {JSONObject[]} JSON objects for that location.
     */
    getAllData(layerName, setName, locationID) {
        if(!this._data[layerName]) return null;
        var results = [];

        for(var i = 0; i < this._data[layerName][setName].length; i++) {

            var entry = this._data[layerName][setName][i];
            if(entry["label"] === locationID || entry["id"] === locationID) {
                // Correct location 
                results.push(entry);
            }
        }
        return results;
    }

    showData(layerName, setName, locationID) {
        // All data entries for this layer an set
        this._selectedData = this.getAllData(layerName, setName, locationID);

        // Create dropdowns to change between tables
        var selectHTML = this.buildComboBox();

        // Setup HTML for results
        DT.sidePanelHandler.setContent(`
            <div id="time-series-container">
                <div id="time-series-title"><b>Location Data</b>
                </div>
                <div id="time-series-control">
                    ` + selectHTML + `
                </div>
                <div id="time-series-chart-container">
                    <canvas id="chart-canvas"></canvas>
                </div>
                <div id="time-series-table-container">
                </div>
            </div>
        `);

        // Show the table
        this.updateTable();

        // Show the chart
        this.updateChart();

        // Hide the legend and footer whilst viewing data
        DT.sidePanelHandler.toggleLegend(false);

        // Hide the footer content, show return link
        var footerContent = document.getElementById("footerContent");
        footerContent.style.display = "none";
        var returnContainer = document.getElementById("returnContainer");
        returnContainer.style.display = "block";
    }

    /**
     * 
     */
    updateTable() {
        var select = document.getElementById("time-series-select");
        var tableName = select.value;

        var tableContainer = document.getElementById("time-series-table-container");
        tableContainer.innerHTML = this.buildTable(tableName);
    }

    /**
     * 
     */
    updateChart() {
        var select = document.getElementById("time-series-select");
        var tableName = select.value;
        this.buildChart(tableName);
    }

    /**
     * 
     */
    updateBoth() {
        this.updateTable();
        this.updateChart();
    }

    /**
     * 
     * @param {*} tableName 
     */
     buildTable(tableName) {
        // Find the correct data entry
        var data = null;
        this._selectedData.forEach(entry => {
            if(entry["name"] === tableName) {
                data = entry;
                return;
            }
        });

        if(data == null) {
            console.log("ERROR: Could not find data entry!");
            return;
        }

        // Get the data for the table
        var independents = data["times"];
        var dependents = data["values"];

        // Build the HTML for the table
        var tableHTML = `<table class="time-series-table" width="100%">`;
        tableHTML += `<tr><th>Time</th><th>` + tableName + `</th></tr>`;

        for(var r = 0; r < independents.length; r++) {
            // Build HTML for one row
            var independent = independents[r];
            var dependent = dependents[r];

            tableHTML += `
                <tr>
                    <td width="50%">
                        ` + independent + `
                    </td>
                    <td width="50%">
                        ` + dependent + `
                    </td>
                </tr>
            `;
        }
        
        tableHTML += `</table>`;
        return tableHTML;
    }

    buildChart(tableName) {
        // Find the correct data entry
        var data = null;
        this._selectedData.forEach(entry => {
            if(entry["name"] === tableName) {
                data = entry;
                return;
            }
        });

        if(data == null) {
            console.log("ERROR: Could not find data entry!");
            return;
        }

        // Get data for plotting
        var independents = data["times"];
        var dependents = [];

        for(var i = 0 ; i < independents.length; i++) {
            dependents.push({
                x: independents[i],
                y: (!isNaN(data["values"][i])) ? data["values"][i] : Number(data["values"][i])
            });
        }

        // Destroy the current chart
        if(this._currentChart != null) {
            this._currentChart.destroy();

        }
        // Create the new chart element
        var ctx = document.getElementById("chart-canvas").getContext("2d");
        this._currentChart = new Chart(ctx, 
            {
                type: "line",
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        legend: {
                            display: false
                        }
                    },
                    scales: {
                        x: {
                            type: 'time',
                            distribution: 'linear',
                            ticks: {
                                font: {
                                    weight: 400,
                                    size: 10
                                }
                            },
                            title: {
                                display: true,
                                text: "Time",
                                font: {
                                    weight: 700,
                                    size: 12
                                }
                            }
                        },
                        y: {
                            ticks: {
                                font: {
                                    weight: 400,
                                    size: 10
                                }
                            },
                            title: {
                                display: true,
                                text: tableName,
                                font: {
                                    weight: 700,
                                    size: 12
                                }
                            }
                        }
                    }
                },
                data: {
                    labels: independents,
                    datasets: [{
                        label: tableName,
                        pointBorderColor: "rgba(33, 150, 243, 0.70)",
                        borderColor: "rgba(33, 150, 243, 0.35)",
                        data: dependents
                    }]
                }
            }
        );
    }

    

    /**
     * 
     * @returns 
     */
    buildComboBox() {
        var selectHTML = `
            <label for="time-series-select">Select a data set:</label>
            <select name="time-series-select" id="time-series-select" onchange="DT.timeSeriesHandler.updateBoth()">
        `;

        this._selectedData.forEach(entry => {
            selectHTML += `
                <option value="` + entry["name"] + `">` + entry["name"] + `</option>
            `;
        });
        selectHTML += `</select>`;
        return selectHTML;
    }

 

}
// End of class.