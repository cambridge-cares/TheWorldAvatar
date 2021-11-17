/**
 * This class handles finding, reading, and storing the metadata that outlines
 * the standard format for Digital Twin visualisation data.
 * 
 * Note that this is the metadata that describes the data itself, not the 
 * metadata that lists additional properties for each feature/location.
 */
class DataRegistry {

    /**
     * Global metadata.
     */
    _globalMeta = [];

    /**
     * Metadata for visualisation data.
     */
    _meta = [];

    /**
     * Returns the metadata for the entire visualisation.
     */
    get globalMeta() {
        return this._globalMeta;
    }

    /**
     * Returns the metadata for the Additional Data sets.
     */
    get meta() {
        return this._meta;
    }

    /**
     * Automatically identify and read metadata if that meets the standard
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
        let topFile = rootDir + "meta.json";

        // Reset data objects
        this._globalMeta = [];
        this._meta = [];

        // Load the top level meta
        var registry = this;
        var promise = $.getJSON(topFile, function(data) {
            // Store global
            registry._globalMeta = data["global"];
            registry._globalMeta["rootDirectory"] = rootDir;

            // Store top level of metadata tree
            registry._meta = data["local"];
        }).promise();

        // Drill down and read the other levels
        promise.then(() => {
            var promises = [];
            var groups = registry._meta["groups"];

            for(var i = 0; i < groups.length; i++) {
                var groupDirectory = rootDir + groups[i]["directory"];
                var groupPromise = registry.#recurseReadMeta(registry, groupDirectory, groups[i]);
                promises.push(groupPromise);
            }
            
            // Once both of those are finished, fire the callback
            Promise.all(promises).then(() => {
                console.log("INFO: Metadata has been loaded.");
                if(callback != null) callback();
            });   
        });
    }

    /**
     * Recurses depth first and returns the first leaf group.
     * 
     * @returns 
     */
    getFirstGroup() {
        var group = [];
        this.#recurseFirstGroup(this._meta, group);
        return group;
    }

    /**
     * Recurse depth wise to find first leaf group
     * @param {*} currentEntry 
     * @param {*} group 
     */
    #recurseFirstGroup(currentEntry, group) {
        if(currentEntry["groups"]) {
            if(currentEntry["groups"]["label"] && currentEntry["groups"]["groups"]) {
                group.push(currentEntry["groups"]["groups"][0]["directory"]);
                this.#recurseFirstGroup(currentEntry["groups"]["groups"][0], group);
            } else {
                group.push(currentEntry["groups"][0]["directory"]);
                this.#recurseFirstGroup(currentEntry["groups"][0], group);
            }
        }
    }

    /**
     * Returns the metadata object for the input grouping (e.g. ["scenario-0", "time-0"]);
     * 
     * @param {String[]} group group definition
     * 
     * @returns {JSONObject} metadata object
     */
    getGroup(group) {
        var result = [];
        this.#findGroup(this._meta, 0, group, result);
        return result[0];
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
    #findGroup(currentGroup, i, groups, result) {
        if(result.length > 0) return;

        if(currentGroup["directory"] === groups[i]) {
            if(i == (groups.length - 1)) {
                result[0] = currentGroup;

            } else if(currentGroup["groups"]["groups"]) {
               for(var j = 0; j < currentGroup["groups"]["groups"].length; j++) {
                    this.#findGroup(currentGroup["groups"]["groups"][j], i + 1, groups, result);
                }
            }

        } else if(currentGroup["groups"]) {
            var subgroups = currentGroup["groups"];
            for(var j = 0; j < subgroups.length; j++) {
                this.#findGroup(subgroups[j], i, groups, result);
            }

        } else if(currentGroup.length > 0) {
            for(var j = 0; j < currentGroup.length; j++) {
                this.#findGroup(currentGroup[j], i, groups, result);
            }
         }
    }

    /**
     * Recursively finds and loads the meta describing the additional
     * data sets.
     * 
     * @param {DataRegistry} registry self
     * @param {string} parentDir parent directory
     * @param {JSONObject} parentEntry parent meta entry
     * 
     * @returns promise
     */
    #recurseReadMeta(registry, parentDir, parentEntry) {
        let metaFile = parentDir + "/meta.json";

        // Read this meta file
        var promise = $.getJSON(metaFile, function(json) {
            return json;
        });

        // Drill down
        return promise.then(
            function(json) {
                if(json["dataSets"]) {
                    parentEntry["dataSets"] = json["dataSets"];    
                    parentEntry["thisDirectory"] = parentDir;    

                } else if(json["groups"]) {
                    parentEntry["thisDirectory"] = parentDir;

                    if(parentEntry.length == 0) {
                        parentEntry.push(json);
                    } else {
                        parentEntry["groups"] = json;
                    }

                    // Recurse into subgroups and read metadata
                    var promises = [];
                    for(var i = 0; i < json["groups"].length; i++) {
                        let groupEntry = json["groups"][i];
                        let groupDir = parentDir + "/" + groupEntry["directory"];
                        promises.push(registry.#recurseReadMeta(registry, groupDir, groupEntry));
                    }
                    return Promise.all(promises);
                } 
            }
        );
    }

}
// End of class.