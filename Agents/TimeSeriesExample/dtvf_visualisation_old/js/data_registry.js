/**
 * This class handles finding, reading, and storing the metadata that outlines
 * the standard format for Digital Twin visualisation data.
 * 
 * Note that this is the metadata that describes the data itself, not the 
 * metadata that lists additional properties for each feature/location.
 */
class DataRegistry {

    /**
     * Overall meta data.
     */
    _overallMeta;

    /**
     * Metadata for fixed data.
     */
    _fixedMeta;

    /**
     * Metadata for additional data.
     */
    _additionalMeta = [];

    /**
     * Returns the metadata for the entire visualisation.
     */
    get overallMeta() {
        return this._overallMeta;
    }

    /**
     * Returns the metadata for the Fixed Data sets.
     */
    get fixedMeta() {
        return this._fixedMeta;
    }

    /**
     * Returns the metadata for the Additional Data sets.
     */
    get additionalMeta() {
        return this._additionalMeta;
    }

    /**
     * Returns the location of the Fixed Data directory.
     */
    getFixedDirectory() {
        let rootDir = this._overallMeta["rootDirectory"];
        return rootDir + this._overallMeta["fixedDirectory"];
    }

    /**
     * Returns the location of the directory represented by the input
     * Additional Data set groups.
     * 
     * @param {string[]} groups 
     */
    getAdditionalDirectory(groups) {
        let rootDir = this._overallMeta["rootDirectory"];
        let additionalDirectory = this._overallMeta["additionalDirectory"];

        return rootDir + additionalDirectory + "/" + groups.join("/");
    }

    /**
     * Returns the metadata for the Additional Data set represented by the
     * input groups.
     * 
     * @param {string[]} groups 
     * 
     * @returns metadata
     */
    getAdditionalGroup(groups) {
        var result = [];
        this.#recurseAdditional(this._additionalMeta, 0, groups, result);
        return result[0];
    }

    /**
     * Automatically identify and load data if that meets the standard
     * format outlined for Digital Twin visualisations.
     * 
     * Note that this method is asynchronous, so a callback should be 
     * passed if action needs to be taken after the data is loaded.
     * 
     * @param {string} rootDir location of root directory containing data
     * @param {function} callback function to call once data is loaded.
     */
    loadMetaData(rootDir, callback) {
        rootDir = (rootDir.endsWith("/")) ? rootDir : rootDir + "/";
        let overallMeta = rootDir + "overall-meta.json";

        // Reset data objects
        this._overallMeta = null;
        this._fixedMeta = null;
        this._additionalMeta = [];

        // Load the overall meta
        var registry = this;
        var promise = $.getJSON(overallMeta, function(json) {
            json["rootDirectory"] =  rootDir;
            registry._overallMeta = json;
        }).promise();

        // After that's loaded, read the fixed and additional meta
        promise.then(
            function() {
                var promises = [];

                // Read the fixed meta
                var fixedPromise = registry.#loadFixedMeta(registry, rootDir);
                promises.push(fixedPromise);
                
                if(registry._overallMeta["additionalDirectory"]) {
                    var selectionsContainer = document.getElementById("selectionsContainer");
                    if(selectionsContainer != null) selectionsContainer.style.display = "block";

                    // If additional directories are present, recursively read that meta
                    var additionalDir = rootDir + registry._overallMeta["additionalDirectory"];
                    var additionalPromise = registry.#loadAdditionalMeta(registry, additionalDir, registry._additionalMeta);
                    promises.push(additionalPromise);
                } else {
                    // No additional data sets, hide the selection controls
                    var selectionsContainer = document.getElementById("selectionsContainer");
                    if(selectionsContainer != null) selectionsContainer.style.display = "none";
                }

                // Once both of those are finished, fire the callback
                Promise.all([promises]).then(() => {
                    console.log("INFO: Metadata for the Fixed and Additional Data sets has been loaded.");
                    if(callback != null) callback();
                });              
            },

            function(error) {
                // Hard fail
                console.log("ERROR: Could not load metadata!");
            }
        );
    }

    /**
     * Recurses the Additional Data groups and finds the group represented
     * by the input group name strings.
     * 
     * @param {JSONObject} currentGroup current entry for searching
     * @param {int} i current depth in groups array
     * @param {string[]} groups names of group
     * @param {JSONObject[]} result container for result
     */
      #recurseAdditional(currentGroup, i, groups, result) {
        if(result.length > 0) return;

        if(currentGroup["directory"] === groups[i]) {
            if(i == (groups.length - 1)) {
                result[0] = currentGroup;

            } else if(currentGroup["groups"]["groups"]) {
               for(var j = 0; j < currentGroup["groups"]["groups"].length; j++) {
                    this.#recurseAdditional(currentGroup["groups"]["groups"][j], i + 1, groups, result);
                }
            }
        } else if(currentGroup["groups"]) {
           for(var j = 0; j < currentGroup["groups"].length; j++) {
                this.#recurseAdditional(currentGroup["groups"][j], i, groups, result);
            }
        } else if(currentGroup.length > 0) {
            for(var j = 0; j < currentGroup.length; j++) {
                this.#recurseAdditional(currentGroup[j], i, groups, result);
            }
         }
    }

    /**
     * Loads the metadata that describes the fixed data set.
     * 
     * @param {DigitalTwinDataLoader} self self
     * @param {string} rootDir root directory of data structures.
     * 
     * @returns promise
     */
    #loadFixedMeta(self, rootDir) {
        let fixedDirectory = self._overallMeta["fixedDirectory"];
        let fixedMetaFile = rootDir + fixedDirectory + "/meta.json";

        return $.getJSON(fixedMetaFile, function(json) {
            self._fixedMeta = json;
        }).promise();
    }


    /**
     * Recursively finds and loads the meta describing the additional
     * data sets.
     * 
     * @param {DigitalTwinDataLoader} self self
     * @param {string} parentDir parent directory
     * @param {JSONObject} parentEntry parent meta entry
     * 
     * @returns promise
     */
    #loadAdditionalMeta(self, parentDir, parentEntry) {
        let metaFile = parentDir + "/meta.json";
        var promise = $.getJSON(metaFile, function(json) {
            return json;
        });

        return promise.then(
            function(json) {
               
                if(json["dataSets"]) {
                    parentEntry["dataSets"] = json["dataSets"];    

                } else if(json["groups"]) {
                    if(parentEntry.length == 0) {
                        parentEntry.push(json);
                    } else {
                        parentEntry["groups"] = json;
                    }

                    var promises = [];
                    for(var i = 0; i < json["groups"].length; i++) {
                        let groupEntry = json["groups"][i];
                        let groupDir = parentDir + "/" + groupEntry["directory"];
                        
                        promises.push(self.#loadAdditionalMeta(self, groupDir, groupEntry));
                    }
                    return Promise.all(promises);
                } 
            }
        );
    }

}
// End of class.