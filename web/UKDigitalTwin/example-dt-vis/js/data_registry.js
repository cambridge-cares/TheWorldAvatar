/**
 * This class handles finding, reading, and storing the metadata that outlines
 * the standard format for Digital Twin visualisations.
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
     * 
     * @param {*} currentGroup 
     * @param {*} i 
     * @param {*} groups 
     * @returns 
     */
    #recurseAdditional(currentGroup, i, groups, result) {
        if(result.length > 0) return;

        if(currentGroup["name"] === groups[i]) {
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
     * Automatically idenfiy and load data if that meets the standard
     * format outlined for Digital Twin visualisations.
     * 
     * Note that this method is asynchronus, so a callback should be 
     * passed if action needs to be taken after the data is loaded.
     * 
     * @param {string} overallMeta location of "overall-meta.json". 
     * @param {function} callback function to call once data is loaded.
     */
    loadMetaData(overallMeta, callback) {
        let rootDir = overallMeta.substring(0, overallMeta.lastIndexOf("/") + 1);

        var registry = this;
        var promise = $.getJSON(overallMeta, function(json) {
            json["rootDirectory"] = rootDir;
            registry._overallMeta = json;
        }).promise();

        promise.then(
            function() {
                var promises = [];
                var fixedPromise = registry.#loadFixedMeta(registry, rootDir);
                promises.push(fixedPromise);
                
                if(registry._overallMeta["additionalDirectory"]) {
                    var additionalDir = rootDir + registry._overallMeta["additionalDirectory"];
                    var additionalPromise = registry.#loadAdditionalMeta(registry, additionalDir, registry._additionalMeta);
                    promises.push(additionalPromise);
                } else {
                    // No additional data sets, hide the selection controls
                    document.getElementById("selectionsContainer").style.display = "none";
                }
            
                Promise.all([promises]).then(() => {
                    console.log("INFO: Metadata for the Fixed Data sets has been loaded.");
                    console.log("INFO: Metadata for the Additional Data sets has been loaded.");
                    if(callback != null) callback();
                });              
            },
            function(error) {
                console.log("ERROR: Could not load metadata!");
            }
        );
    }

    /**
     * Loads the metadata that describes the fixed data set.
     * 
     * @param {DigitalTwinDataLoader} self 
     * @param {string} rootDir root directory of data structures.
     */
    #loadFixedMeta(self, rootDir) {
        let fixedDirectory = self._overallMeta["fixedDirectory"];
        let fixedMetaFile = rootDir + fixedDirectory + "/meta.json";

        return $.getJSON(fixedMetaFile, function(json) {
            self._fixedMeta = json;
        }).promise();
    }


    /**
     * 
     * @param {*} self 
     * @param {*} parentDir 
     * @param {*} parentEntry 
     * @returns 
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