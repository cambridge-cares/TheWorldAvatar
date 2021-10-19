/**
 * This class handles finding, reading, and storing the metadata that outlines
 * the standard format for Digital Twin visualisations.
 */
class DigitalTwinDataRegistry {

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
    _additionalMeta = {};

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
        return this.#recurseAdditional(this._additionalMeta, 0, groups);
    }

    /**
     * 
     * @param {*} currentGroup 
     * @param {*} i 
     * @param {*} groups 
     * @returns 
     */
    #recurseAdditional(currentGroup, i, groups) {
        if(i >= groups.length) return currentGroup;

        if(currentGroup[groups[i]] != null) {
            return this.#recurseAdditional(currentGroup[groups[i]], i + 1, groups);
        } else {
            return null;
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
                var fixedPromise = registry.#loadFixedMeta(registry, rootDir);
                var additionalPromises = registry.#loadAdditionalMeta(registry, rootDir);

                var allPromises = additionalPromises.concat([fixedPromise]);
                Promise.all(allPromises).then(() => {
                    console.log("INFO: All metadata has now been loaded.");
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

        var promise = $.getJSON(fixedMetaFile, function(json) {
            self._fixedMeta = json;
            console.log("INFO: Metadata for the Fixed Data sets has been loaded.");
        });

        return promise;     
    }
   
    /**
     * Loads the metadata that describes the additional data sets.
     * 
     * @param {DigitalTwinDataLoader} self 
     * @param {string} rootDir root directory of data structures.
     */
    #loadAdditionalMeta(self, rootDir) {
        let additionalRoot = self._overallMeta["additionalDirectory"];
        let additionalDirectories = self._overallMeta["additionalSubdirectories"];

        var promises = []
        additionalDirectories.forEach(additionalDirectory => {
            let additionalMeta = rootDir + additionalRoot + "/" + additionalDirectory + "/meta.json";
            let directories = additionalDirectory.split("/");

            try {
                promises.push($.getJSON(additionalMeta, function(json) {
                    var additionalMeta = json;
                    self.#buildAdditionalTree(self._additionalMeta, directories, 0, additionalMeta);
                    console.log("INFO:  Metadata for Additional Data set at '" + additionalDirectory + "' has been loaded.");
                }));
            } catch(error) {
                console.log("ERROR: Could not find/read 'meta.json' files for additional directory.");
            }       
        });
        return promises;
    }

    /**
     * Recursively builds a tree structure for the additional
     * data directories.
     * 
     * @param {array} parent target array
     * @param {string[]} directories directory stucture
     * @param {int} i current level
     * @param {*} last object to store at final level
     */
    #buildAdditionalTree(parent, directories, i, last) {
        if(i == (directories.length - 1)) {
            if(!parent[directories[i]]) parent[directories[i]] = last;
        } else {
            if(!parent[directories[i]]) parent[directories[i]] = {};
            this.#buildAdditionalTree(parent[directories[i]], directories, i + 1, last);
        }
    }


}