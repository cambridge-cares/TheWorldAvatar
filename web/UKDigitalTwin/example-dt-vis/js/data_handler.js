/**
 * This class uses the metadata discovered by the DigitalTwinDataRegistry
 * to read the location GeoJSON files and add them to the MapBox
 * map object as a new data source.
 */
class SourceHandler {

    /**
     * DigitalTwinDataRegistry instance.
     */
    _dataRegistry;

    /**
     * MapBox map
     */
    _map;

    _currentAdditionals = [];


    /**
     * Initialise a new DigitalTwinDataHandler.
     * 
     * @param {*} dataRegistry DigitalTwinDataRegistry
     * @param {*} map MapBox map 
     */
    constructor(dataRegistry, map) {
        this._dataRegistry = dataRegistry;
        this._map = map;
    }

    /**
     * Adds the Fixed Data set location files as MapBox sources.
     */
    addFixedSources() {
        let fixedMeta = this._dataRegistry.fixedMeta;
        let fixedDataSets = fixedMeta["dataSets"];

        fixedDataSets.forEach(fixedDataSet => {
            let name = fixedDataSet["name"];
            let locationFile = fixedDataSet["locationFile"];
            locationFile = this._dataRegistry.getFixedDirectory() + "/" + locationFile;

            this._map.addSource(name, {
                type: "geojson",
                data: locationFile
            });
            console.log("INFO: Added fixed '" + name + "' source to MapBox.");
        });
    }

    /**
     * Adds the MapBox sources corresponding to the Additional Data sets
     * represented by the input groups.
     * 
     * @param {string[]} groups
     */
    addAdditionalSources(groups) {
        if(this._currentAdditionals.includes(groups)) {
            // Already plotted, skip
            return;
        }

        console.log("Plotting: " + groups);
        this._currentAdditionals.push(groups);
        let result = this._dataRegistry.getAdditionalGroup(groups);
        console.log(result);
        
        if(result != null) {
            let group = result["group"];
            let name = group["name"];

            let directory = this._dataRegistry.getAdditionalDirectory(groups);
            let locationFile = group["locationFile"];
            locationFile = directory + "/" + locationFile;

            if(this._map.getSource(name) == null) {
                this._map.addSource(name, {
                    type: "geojson",
                    data: locationFile
                });
                console.log("INFO: Added additional '" + name + "' source to MapBox.");
            }
        }
    }

    /**
     * Removes the MapBox sources corresponding to the Additional Data sets
     * represented by the input groups.
     * 
     * @param {string[]} groups
     */
     removeAdditionalSources(groups) {
        if(this._currentAdditionals.includes(groups)) {
            this._currentAdditionals.remove(groups);
        }

        let result = this._dataRegistry.getAdditionalGroup(groups);

        if(result != null) {
            let group = result["group"];
            let name = group["name"];
            
            if(this._map.getSource(name) != null) {
                this.#removeLayersWithSource(name, this._map);
                mapBox.removeSource(name);
            }
        }
    }

    /**
     * Remove all layers currently on the map that use the input source name.
     * 
     * @param {*} sourceName source name
     */
    #removeLayersWithSource(sourceName) {
        let allLayers = this._map.getStyle().layers;

        for(var i = (layers.length - 1); i >= 0; i--) {
            let layerSource = allLayers[i]["source"];
            if(layerSource === sourceName) this._map.removeLayer(allLayers[i]["id"]);
        }
    }

   
}