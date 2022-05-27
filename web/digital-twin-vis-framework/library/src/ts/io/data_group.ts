/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataGroup {

    public parentGroup: DataGroup;

    public dataSources: Array<DataSource> = [];

    public dataLayers: Array<DataLayer> = [];

    public subGroups: Array<DataGroup> = [];

    public name: string;

    public subLabel: string;

    public mapOptions: Object;

    // Unique, dynamically generated, ID
    public id: string;

    constructor() {
        // Empty
    }

    /**
     * Parses the definition of each data set defined within the group. Note that
     * this just parse the definitions to create instance objects, it may not
     * read the full data at this point.
     * 
     * @param sourceJSON JSON array of source nodes.
     */
    public parseDataSources(sourcesJSON) {
        for(var i = 0; i < sourcesJSON.length; i++) {
            let node = sourcesJSON[i];

            // Create and store source
            let source = new DataSource(node);
            this.dataSources.push(source);
        }
    }

    /**
     * Parses the definition of each data layer defined within the group. Note that
     * this just parse the definitions to create instance objects, it may not
     * plot the layer at this point.
     * 
     * This method relies on the presumption that data sources have been parsed first.
     * 
     * @param layersJSON JSON array of layer nodes.
     */
    public parseDataLayers(layersJSON) {
        if(this.dataSources.length === 0) {
            throw new Error("Data sources must be parsed before parsing data layers.");
        }

        for(var i = 0; i < layersJSON.length; i++) {
            let node = layersJSON[i];
            let source = this.getSourceWithID(node["source"]);

            let layer = null;
            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    let layerID = this.id + "." + node["id"];
                    layer = new MapBoxLayer(layerID, node["name"], source);
                break;
    
                default:
                    throw new Error("Unknown map provider specified!");
                break;
            }
           
            layer.definition = node;
            this.dataLayers.push(layer);
        }
    }

    /**
     * Returns the DataSource instance with the input name if present. Note that this
     * only searches for sources directly within this group (i.e. no subgroups).
     * 
     * @param name name of target data source.
     * 
     * @return matching DataSource, null if not present.
     */
    public getSourceWithID(id: string): DataSource {
          return this.dataSources.find(source => source.id === id);
    }

    /**
     * Returns the DataLayer instance with the input name if present. Note that this
     * only searches for layers directly within this group (i.e. no subgroups).
     * 
     * @param name name of target data layer.
     * 
     * @return matching DataLayer, null if not present.
     */
    public getLayerWithID(id: string): DataLayer {
        return this.dataLayers.find(layer => layer.id === id);
    }

    /**
     * Working from this group up through the tree, collect and
     * return the list of all defined DataSource instances.
     * 
     * @returns all data sources from this group and it's parents
     */
    public flattenUp() {
        let flatLayers = [];
        this.recurseFlattenUpLayers(flatLayers, this);
        return flatLayers;
    }
    
    /**
     * Recursively work up the group tree to collect DataLayer instances.
     */
    private recurseFlattenUpLayers(array, currentGroup) {
        if (currentGroup !== null && currentGroup !== undefined) {
            currentGroup.dataLayers.forEach((dataLayer) => {
                array.push(dataLayer);
            });

            if(currentGroup.parentGroup !== null) {
                this.recurseFlattenUpLayers(array, currentGroup.parentGroup);
            }
        }
    }
}
