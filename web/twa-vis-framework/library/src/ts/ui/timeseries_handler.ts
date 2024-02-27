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
    public parseData(json) {
        this._selectedData = [];
        let entries = JSONFormatter.formatJSON(json);

        if(!Array.isArray(entries)) {
            // Not a JSON Array, wrap in one
            let temp = [];
            temp.push(entries);
            entries = temp;
        }

        // Parse raw JSON into expanded form
        for(const element of entries) {
            let entry = element;

            if(!entry?.["data"] || !entry?.["units"] || !entry?.["time"] || !entry?.["values"]){
                // Skip if any required properties are missing
                continue;
            }

            // May have multiple data sets with differing units
            let tableNames = entry["data"];
            let tableUnits = entry["units"];

            for(let j = 0; j < tableNames.length; j++) {
                // Dependent name
                let tableName = tableNames[j];

                // Dependent unit
                let tableUnit = tableUnits[j];
                if(tableUnit != null && tableUnit !== "null") {
                    tableName += " [" + tableUnit + "]";
                }
                
                // Get timestamps
                let tableTimes = entry["time"];

                // Condition time format
                let timeClass = null
                let firstTime = tableTimes[0];

                // Attempt to determine if time values are dates, times, or numbers
                timeClass = this.determineTimeValueType(entry, firstTime, timeClass);
                
                // Align time series formats
                // dateTime / Instant: "YYYY-MM-DD HH:mm:ss"
                // offsetTime: "HH:mm:ss"
                // All times are local times!!
                this.formatTimeSeries(tableTimes, timeClass, entry, j, tableName, tableUnit);
            }            
        }
    }

    private determineTimeValueType(entry: any, firstTime: any, timeClass: any) {
        if (undefined == entry["timeClass"]) {
            if (isNaN(Date.parse(firstTime)) && isNaN(Date.parse(firstTime.replaceAll("-", "/")))) {
                if (firstTime.includes(":")) {
                    timeClass = "offsetTime";
                }
                else {
                    timeClass = "number";
                }
            }
            else {
                timeClass = "dateTime";
            }
        } else { timeClass = entry["timeClass"]; }
        return timeClass;
    }

    private formatTimeSeries(tableTimes: any, timeClass: any, entry: any, j: number, tableName: any, tableUnit: any) {
        for (let t = 0; t < tableTimes.length; t++) {
            // dateTime/Instant format
            if (timeClass === "dateTime" || entry["timeClass"] === "Instant") {
                // @ts-ignore
                tableTimes[t] = moment(tableTimes[t], "YYYY-MM-DDTHH:mm:ss").format("YYYY-MM-DD HH:mm:ss");
            }

            // OffsetTime format
            else if (timeClass === "offsetTime") {
                // @ts-ignore
                tableTimes[t] = moment(tableTimes[t], "HH:mm:ss").format("HH:mm:ss");
            }
        }

        // Get correct value array
        let tableValues = entry["values"][j];

        // Store
        this._selectedData.push({
            "name": tableName,
            "id": this._selectedData.length,
            "unit": tableUnit,
            "times": tableTimes,
            "values": tableValues,
            "timeClass": timeClass,
            "valuesClass": (entry["valuesClass"]) ? entry["valuesClass"][j] : "Number"
        });
    }

    /**
     * Build and show the first time series data set.
     * 
     * @param {String} containerElement name of div to add controls to.
     */
    public showData(containerElement) {
        // Create dropdowns to change between tables
        let selectHTML = this.buildComboBox();

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
    }

    /**
     * Update the table contents with the input set.
     * 
     * @param {String} setName data set name
     */
    public updateTable(setName) {
        let tableContainer = document.getElementById("time-series-table-container");
        tableContainer.innerHTML = this.buildTable(setName);
    }

    /**
     * Update the chart contents with the input set.
     * 
     * @param {String} setName data set name
     */
    public updateChart(setName) {
        this.buildChart(setName);
    }

    /**
     * Update both the table and chart contents.
     * 
     * @param {Integer} setID timeseries ID
     */
    public update(setID) {
        // Find the correct data entry
        let data = null;
        this._selectedData.forEach(entry => {
            if(data === null && entry["id"] === +setID) {
                data = entry;
            }
        });
        if(data == null) return;

        // Update table
        this.updateTable(setID);

        // Update chart if values list is not String
        let chartContainer = document.getElementById("time-series-chart-container");
        if(data["valuesClass"] !== "String") {
            this.updateChart(setID);
            chartContainer.style.display = "block";
        } else {
            chartContainer.style.display = "none";
        }
    }

    /**
     * Build the table containing raw timeseries data.
     * 
     * @param {Integer} setID timeseries ID
     */
    public buildTable(setID) {
        // Find the correct data entry
        let data = null;
        this._selectedData.forEach(entry => {
            if(data === null && entry["id"] === +setID) {
                data = entry;
            }
        });
        if(data == null) return;

        // Get the data for the table
        let independents = data["times"];
        let dependents = data["values"];

        // Build the HTML for the table
        let tableHTML = `<table class="time-series-table" width="100%">`;
        tableHTML += `<tr><th>Time</th><th>` + data["name"] + `</th></tr>`;

        for(let r = 0; r < independents.length; r++) {
            // Build HTML for one row
            let independent = independents[r];
            let dependent = dependents[r];

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
     * @param {Integer} setID timeseries ID
     */
    public buildChart(setID) {
        // Find the correct data entry
        let data = null;
        this._selectedData.forEach(entry => {
            if(data === null && entry["id"] === +setID) {
                data = entry;
            }
        });
        if(data == null) return;

        // Get data for plotting
        let independents = [...data["times"]];
        let dependents = [];

        for(let i = 0 ; i < independents.length; i++) {

            if(data["timeClass"] === "dateTime" || data["timeClass"] === "Instant") {
                // @ts-ignore
                independents[i] = moment(independents[i], "YYYY-MM-DD HH:mm:ss");

            } else if(data["timeClass"] === "offsetTime") {
                // @ts-ignore
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
        // @ts-ignore
        let ctx = document.getElementById("chart-canvas").getContext("2d");

        // Determine axis types
        let yAxisType = ("Boolean" === data["valuesClass"]) ? "category" : "linear";
        let xAxisType = "linear";
        if(data["timeClass"] === "dateTime" || data["timeClass"] === "offsetTime") {        
            xAxisType = "time";
        }

        // Create the chart object
        // @ts-ignore
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
                                text: "Date/Time",
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
                                text: data["name"],
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
                        label: data["name"],
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
    public buildComboBox() {
        let selectHTML = `
            <label for="time-series-select">Select a data set:</label>
            <select name="time-series-select" id="time-series-select" onchange="manager.updateTimeseries(this.value)">
        `;

        this._selectedData.forEach(entry => {
            selectHTML += `
                <option value="` + entry["id"] + `">` + entry["name"] + `</option>
            `;
        });
        selectHTML += `</select>`;
        return selectHTML;
    }

}
// End of class.