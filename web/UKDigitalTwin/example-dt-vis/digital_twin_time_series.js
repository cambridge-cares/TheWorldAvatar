/**
 * Handles reading and plotting standard Time Series data.
 */
class DigitalTwinTimeSeries {

    // Loaded time series data
    _data = [];


    /**
     * Reads the input data file and stores it under the input set name.
     * 
     * @param {String} setName name to give this data set
     * @param {String} jsonFile JSON file containing data
     */
    readFile(setName, jsonFile) {
        var that = this;

		var rawFile = new XMLHttpRequest();
		rawFile.onreadystatechange = function() {

			if (rawFile.readyState == 4 && rawFile.status == "200") {
				console.log("INFO: Reading TimeSeries data from file at: " + jsonFile);
				that._data[setName] = JSON.parse(rawFile.responseText);
				console.log("INFO: TimeSeries data has been read.");
			} 
		}
		rawFile.open("GET", jsonFile, true);
		rawFile.send();
    }


    /**
     * 
     * @param {String} dataSetName name of data set to look within.
     * @param {String} dataName value of the 'data' field within the JSON
     * @param {Integer} locationID value of the 'id' field within the JSON
     * 
     * @returns {JSONObject} JSON object for that location and data set.
     */
    getData(dataSetName, dataName, locationID) {
        if(!this._data[dataSetName]) return null;
        console.log("INFO: Looking for '" + dataName + "' entry for ID '" + locationID + "' within the '" + dataSetName + "' data set.");

        for(var i = 0; i < this._data[dataSetName].length; i++) {
            var entry = this._data[dataSetName][i];

            if(entry["id"] === locationID) {
                // Correct location 

                if(entry["data"] === dataName || entry["data"].includes(dataName)) {
                    // Correct data type
                    return entry;
                }
            }
        }

        return null;
    }

    /**
     * 
     * @param {String} dataSetName name of data set to look within.
     * @param {Integer} locationID value of the 'id' field within the JSON
     * 
     * @returns {JSONObject[]} JSON objects for that location.
     */
    getAllData(dataSetName, locationID) {
        if(!this._data[dataSetName]) return null;
        var results = [];
        console.log("INFO: Looking for all entries for ID '" + locationID + "' within the '" + dataSetName + "' data set.");

        for(var i = 0; i < this._data[dataSetName].length; i++) {
            var entry = this._data[dataSetName][i];

            if(entry["label"] === locationID || entry["id"] === locationID) {
                // Correct location 
                results.push(entry);
            }
        }
        return results;
    }

    buildTables(dataEntries) {
        
    }

}
// End of class.