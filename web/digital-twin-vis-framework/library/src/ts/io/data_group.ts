/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataGroup {

    public parentGroup: DataGroup;

    public dataSources: Array<DataSource> = [];

    public dataLayers: Array<DataLayer> = [];

    public subGroups: Array<DataGroup> = [];

    public name: string;

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

            let sourceID = this.id + "." + node["id"];
            source.id = sourceID;

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
            node["source"] = this.id + "." + node["source"];

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
        let array = [];
        this.recurseFindSource(array, this, id);
        return (array.length === 1) ? array[0] : null;
    }

    /**
     */
    private recurseFindSource(array, currentGroup, target) {
        let source =  this.dataSources.find(source => source.id === target);

        if(source === null || source === undefined) {
            currentGroup.subGroups.forEach(subGroup => {
                this.recurseFindSource(array, subGroup, target);
            });
        } else {
            array.push(source);
        }
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
        let array = [];
        this.recurseFindLayer(array, this, id);
        return (array.length === 1) ? array[0] : null;
    }

    /**
     */
    private recurseFindLayer(array, currentGroup, target) {
        let layer = currentGroup.dataLayers.find(layer => layer.id === target);

        if(layer === null || layer === undefined) {
            currentGroup.subGroups.forEach(subGroup => {
                this.recurseFindLayer(array, subGroup, target);
            });
        } else {
            array.push(layer);
        }
    }

    /**
     * Working from this group down through the tree, collect and
     * return the list of all defined DataSource instances.
     * 
     * @returns all data sources from this group and its children
     */
    public flattenDown() {
        let flatLayers = [];
        this.recurseFlattenDownLayers(flatLayers, this);
        return flatLayers;
    }
    
    /**
     * Recursively work down the group tree to collect DataLayer instances.
     */
    private recurseFlattenDownLayers(array, currentGroup) {
        if (currentGroup !== null && currentGroup !== undefined) {
            currentGroup.dataLayers.forEach((dataLayer) => {
                array.push(dataLayer);
            });

            currentGroup.subGroups.forEach(subGroup => {
                this.recurseFlattenDownLayers(array, subGroup);
            });
        }
    }
}
