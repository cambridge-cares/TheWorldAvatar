/**
 * Handles reading and plotting standard Time Series data.
 */
class TimeseriesHandler {

    // Current data set for the selected layer, set, and location
    _selectedData;

    //
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
                for(var t = 0; t < tableTimes.length; t++) {
                    tableTimes[t] = tableTimes[t].replace("T", " ").replace("Z", "");
                }

                // Get correct value array
                var tableValues = entry["values"][j];

                // Store
                this._selectedData.push({
                    "name": tableName,
                    "id": entry["id"],
                    "unit": tableUnit,
                    "times": tableTimes,
                    "values": tableValues
                });
            }            
        }
    }

    /**
     * 
     * @param {*} containerElement 
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
     * 
     */
    updateTable(setName) {
        var tableContainer = document.getElementById("time-series-table-container");
        tableContainer.innerHTML = this.buildTable(setName);
    }

    /**
     * 
     */
    updateChart(setName) {
        this.buildChart(setName);
    }

    /**
     * 
     */
    update(setName) {
        this.updateTable(setName);
        this.updateChart(setName);
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