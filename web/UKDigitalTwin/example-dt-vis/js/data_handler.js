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
            let dataSets = result["dataSets"];

            for(var i = 0; i < dataSets.length; i++) {
                let dataSet = dataSets[i];
                if(!dataSet["locationFile"]) continue;

                let name = dataSet["name"];
                let directory = this._dataRegistry.getAdditionalDirectory(groups);

                let locationFile = dataSet["locationFile"];
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
            let dataSets = result["dataSets"];

            for(var i = 0; i < dataSets.length; i++) {
                let dataSet = dataSets[i];
                if(!dataSet["locationFile"]) continue;

                let name = dataSet["name"];
                if(this._map.getSource(name) != null) {
                    this._map.removeSource(name);
                    console.log("INFO: Source '" + name + "' has been removed.");
                }
            }
        }
    }

}