/**
 * Handles reading and plotting standard Time Series data.
 */
class TimeseriesHandler {

    // Current data set for the selected layer, set, and location.
    _selectedData;

    // Current chart object.
    _currentChart;

    /**
     * Expands the raw time series JSON and stores it in a format that easier to
     * pipe into tables and charts later.
     * 
     * @param {JSONObject[]} entries 
     */
    parseData(entries) {
        this._selectedData = [];

        // Parse raw JSON into expanded form
        for(var i = 0; i < entries.length; i++) {
            var entry = entries[i];

            if(entry == null || !entry["data"] || !entry["units"] || !entry["time"] || !entry["values"]){
                // Skip if any required properties are missing
                continue;
            }

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

                // Condition time format
                var timeClass = null
                var firstTime = tableTimes[0];

                // Regex tokens to test before and after conversion to Moment format
                var regexBeforeMoment = String(firstTime).match(/^\d{4}-\d{2}-\d{2}T\d{2}(:\d{2}){1,2}Z/);
                var regexAfterMoment = String(firstTime).match(/^\d{4}-\d{2}-\d{2}\s\d{2}(:\d{2}){1,2}/);

                if(regexBeforeMoment || regexAfterMoment) {
                    timeClass = "dateTime"
                } else if(String(firstTime).includes(":")) {
                    timeClass = "offsetTime"
                } else {
                    timeClass = "number";
                }
                
                // Align time series formats
                // dateTime / Instant: "YYYY-MM-DD HH:mm:ss"
                // offsetTime: "HH:mm:ss"
                // All times are local times!!
                for(var t = 0; t < tableTimes.length; t++) {
                    // dateTime/Instant format
                    if(timeClass === "dateTime") {
                        tableTimes[t] = moment(tableTimes[t], "YYYY-MM-DDTHH:mm:ss").format("YYYY-MM-DD HH:mm:ss");
                    }
                    // OffsetTime format
                    else if(timeClass === "offsetTime") {
                        tableTimes[t] = moment(tableTimes[t], "HH:mm:ss").format("HH:mm:ss");
                    }
                }

                // Get correct value array
                var tableValues = entry["values"][j];

                // Store
                this._selectedData.push({
                    "name": tableName,
                    "id": entry["id"],
                    "unit": tableUnit,
                    "times": tableTimes,
                    "values": tableValues,
                    "timeClass": timeClass,
                    "valuesClass": (entry["valuesClass"]) ? entry["valuesClass"][j] : "Number"
                });
            }            
        }
    }

    /**
     * Build and show the first time series data set.
     * 
     * @param {String} containerElement name of div to add controls to.
     */
    showData(containerElement) {
        // Create dropdowns to change between tables
        var selectHTML = this.buildComboBox();

        // Setup HTML for results
        document.getElementById(containerElement).innerHTML = `
            <div id="time-series-control">
                ` + selectHTML + `
            </div>
            <div id="time-series-chart-container">
                <canvas id="chart-canvas"></canvas>
            </div>
            <div id="time-series-table-container">
            </div>
        `;

        // Auto-select the first option in the dropdown
        document.getElementById("time-series-select").onchange();
    }

    /**
     * Update the table contents with the input set.
     * 
     * @param {String} setName data set name
     */
    updateTable(setName) {
        var tableContainer = document.getElementById("time-series-table-container");
        tableContainer.innerHTML = this.buildTable(setName);
    }

    /**
     * Update the chart contents with the input set.
     * 
     * @param {String} setName data set name
     */
    updateChart(setName) {
        this.buildChart(setName);
    }

    /**
     * Update both the table and chart contents.
     * 
     * @param {String} setName data set name
     */
    update(setName) {
        this.updateTable(setName);
        this.updateChart(setName);
    }

    /**
     * Build the table containing raw timeseries data.
     * 
     * @param {String} tableName name of the table.
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
        if(data == null) return;

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

    /**
     * Build the chart displaying timeseries data.
     * 
     * @param {String} tableName name of the table.
     */
    buildChart(tableName) {
        // Find the correct data entry
        var data = null;
        this._selectedData.forEach(entry => {
            if(entry["name"] === tableName) {
                data = entry;
                return;
            }
        });
        if(data == null) return;

        // Get data for plotting
        var independents = [...data["times"]];
        var dependents = [];

        for(var i = 0 ; i < independents.length; i++) {
            if(data["timeClass"] === "dateTime") {
                independents[i] = moment(independents[i], "YYYY-MM-DD HH:mm:ss");
            } else if(data["timeClass"] === "offsetTime") {
                independents[i] = moment(independents[i], "HH:mm:ss");
            }

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

        // Determine axis types
        var yAxisType = ("Boolean" === data["valuesClass"]) ? "category" : "linear";
        var xAxisType = "linear";
        if(data["timeClass"] === "dateTime" || data["timeClass"] === "offsetTime") {        
            xAxisType = "time";
        }

        // Create the chart object
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
                            type: xAxisType,
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
                            type: yAxisType,
                            labels: [true, false],
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
     * Builds the drop-down box to change the data set.
     * 
     * @returns html component
     */
    buildComboBox() {
        var selectHTML = `
            <label for="time-series-select">Select a data set:</label>
            <select name="time-series-select" id="time-series-select" onchange="manager.updateTimeseries(this.value)">
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