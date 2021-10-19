/**
 * This class uses the metadata discovered by the DigitalTwinDataRegistry
 * to read the location GeoJSON files and add them to the MapBox
 * map object as a new data source.
 */
class DigitalTwinDataHandler {

    /**
     * DigitalTwinDataRegistry instance.
     */
    _dataRegistry;

    /**
     * MapBox map
     */
    _map;


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
        let result = this._dataRegistry.getAdditionalGroup(groups);

        if(result != null) {
            let scenario = result["scenario"];
            let name = scenario["name"];

            let directory = this._dataRegistry.getAdditionalDirectory(groups);
            let locationFile = scenario["locationFile"];
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
        let result = this._dataRegistry.getAdditionalGroup(groups);

        if(result != null) {
            let scenario = result["scenario"];
            let name = scenario["name"];
            
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