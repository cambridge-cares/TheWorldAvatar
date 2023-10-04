/**
 * This class handles the addition of sources to the MapBox map.
 */
class SourceHandler {

    // MapBox map.
    _map;

    // List of source IDs currently on map.
    _currentSources = [];

    // DataRegistry object.
    _registry;

    /**
     * Initialise a new SourceHandler.
     * 
     * @param {?} map MapBox map 
     */
    constructor(map, registry) {
        this._map = map;
        this._registry = registry;
    }

    /**
     * Adds 3D terrain provided by MapBox. 
     * WARNING: This may not be compatible with 3D building data unless the
     * building's base height has been set correctly.
     */
    set3DTerrain(enabled) {
        if(enabled) {
            this._map.addSource('mapbox-3d-terrain', {
                type: 'raster-dem',
                url: 'mapbox://mapbox.mapbox-terrain-dem-v1',
                tileSize: 512,
                maxzoom: 14
            });
            this._map.setTerrain({
                source: 'mapbox-3d-terrain', 
                exaggeration: 1.5
            });
        } else {
            this._map.setTerrain(null);
            if(this._map.getSource("mapbox-3d-terrain") != null) this._map.removeSource('mapbox-3d-terrain');
        }
    }

    /**
     * Generates a new MapBox source from the definition taken
     * from the meta.json file.
     * 
     * @param {String} rootDir absolute directory containing data 
     * @param {JSONObject} dataSet data set definition
     */
    addSource(rootDir, dataSet) {
        try {
            let name = dataSet["name"];
            let type = dataSet["dataType"];
            let location = dataSet["dataLocation"];

            // If the data location is NOT a URL, it will be a relative file path.
            // If that's the case, make it absolute
            if(!this.#isURL(location)) {
                location = (rootDir.endsWith("/")) ? (rootDir + location) : (rootDir + "/" + location);
            }

            // Add source depending on type
            switch(type) {
                default:
                case "geojson":
                    return this.#addGeoJSONSource(dataSet, name, location);
                case "raster":
                    return this.#addRasterSource(dataSet, name, location);
                case "vector":
                    return this.#addVectorSource(dataSet, name, location);
            }
        } catch(error) {
            console.log("ERROR: Could not load source '" + name + "', it will be skipped.");
        }
    }

    /**
     * Removes all sources with the attribution:cmcl metadata.
     */
    removeSources() {
        for(var i = 0; i < this._currentSources.length; i++) {
            if(this._map.getSource(this._currentSources[i]) != null) {
                this._map.removeSource(this._currentSources[i]);
            }
        }

        this._currentSources = [];
        console.log("INFO: Removed all Sources.");
    }

    /**
     * Very dumb way of working out if the input location string
     * is a remote URL (as opposed to a local file path).
     * 
     * @param {String} location data location 
     * @returns true if URL
     */
    #isURL(location) {
        if(location.startsWith("http")) return true;
        if(location.startsWith("localhost")) return true;
        if(location.startsWith("192")) return true;
        if(location.startsWith("127")) return true;
    }

    /**
     * Given a single dataSet definition, this method adds it as a 
     * GeoJSON source to the MapBox map object.
     * 
     * @param {JSONObject} dataSet definition of data set
     * @param {String} name name of the data set
     * @param {String} location data location (URL or absolute file path)
     */
    #addGeoJSONSource(dataSet, name, location) {
        this._currentSources.push(name);

        // Load the source into memory locally
        var self = this;
        return $.getJSON(location, function(json) {
            // Cache the JSON object (so we can iterate through it later)
            self._registry.cachedGeoJSON[name] = json;

            // Determine source options
            let options = {
                "type": "geojson",
                "data": json,
                "generateId": false,
                "attribution": "CMCL Innovations"
            };

            // Add clustering settings if present in dataSet definition
            if(dataSet["cluster"]) options["cluster"] = dataSet["cluster"];
            if(dataSet["clusterMaxZoom"]) options["clusterMaxZoom"] = dataSet["clusterMaxZoom"];
            if(dataSet["clusterRadius"]) options["clusterRadius"] = dataSet["clusterRadius"];
            if(dataSet["clusterProperties"]) options["clusterProperties"] = dataSet["clusterProperties"];

            // Add to map
            self._map.addSource(name, options);
            console.log("INFO: Added '" + name + "' as a GeoJSON source to MapBox.");
        });
    }

    /**
     * Given a single dataSet definition, this method adds it as a 
     * Raster source to the MapBox map object.
     * 
     * @param {JSONObject} dataSet definition of data set
     * @param {String} name name of the data set
     * @param {String} location data location (URL)
     */
    #addRasterSource(dataSet, name, location) {
        this._currentSources.push(name);

        // Determine source options
        let options = {
            "type": "raster",
            "tiles": [location],
            "attribution": "CMCL Innovations"
        };

        // Add additional raster settings if present in dataSet definition
        if(dataSet["bounds"]) options["bounds"] = dataSet["bounds"];
        //if(dataSet["tiles"]) options["tiles"] = dataSet["tiles"];
        if(dataSet["tileSize"]) options["tileSize"] = dataSet["tileSize"];
        if(dataSet["minzoom"]) options["minzoom"] = dataSet["minzoom"];
        if(dataSet["maxzoom"]) options["maxzoom"] = dataSet["maxzoom"];

        // Add to map
        this._map.addSource(name, options);
        console.log("INFO: Added '" + name + "' as a Raster source to MapBox.");
    }

    /**
     * Given a single dataSet definition, this method adds it as a 
     * Vector source to the MapBox map object.
     * 
     * @param {JSONObject} dataSet definition of data set
     * @param {String} name name of the data set
     * @param {String} location data location (URL)
     */
    #addVectorSource(dataSet, name, location) {
        this._currentSources.push(name);

        // Determine source options
        let options = {
            "type": "vector",
            "data": location,
            "attribution": "CMCL Innovations"
        };

        // Add additional vector settings if present in dataSet definition
        if(dataSet["bounds"]) options["bounds"] = dataSet["bounds"];
        if(dataSet["tiles"]) options["tiles"] = dataSet["tiles"];
        if(dataSet["minzoom"]) options["minzoom"] = dataSet["minzoom"];
        if(dataSet["maxzoom"]) options["maxzoom"] = dataSet["maxzoom"];

        // Add to map
        this._map.addSource(name, options);
        console.log("INFO: Added '" + name + "' as a Vector source to MapBox.");
    }

}
// End of class.