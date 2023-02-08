/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataGroup {

    /**
     * Parent group
     */
    public parentGroup: DataGroup;

    /**
     * Sources
     */
    public dataSources: Array<DataSource> = [];

    /**
     * Layers
     */
    public dataLayers: Array<DataLayer> = [];

    /** 
     * Sub groups
     */
    public subGroups: Array<DataGroup> = [];

    /**
     * Name
     */
    public name: string;

    /**
     * Unique, dynamically generated, ID
     */
    public id: string;

    /**
     * 
     */
    public defaultExpanded: boolean = true; 

    /**
     * Constructor
     */
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
     * @param stack base URL of the connected stack
     * @param layersJSON JSON array of layer nodes.
     */
    public parseDataLayers(stack: string, layersJSON) {
        for(var i = 0; i < layersJSON.length; i++) {
            let node = layersJSON[i];

            let source = this.findSource(node["source"]);
            if(source === null || source === undefined) {
                console.error("Layer with id '" + node["id"] + "' references a source that is not defined, will skip it!");
                continue;
            }
            node["source"] = source.id;
           
            let layer = null;
            let layerID = this.id + "." + node["id"];

            switch(Manager.PROVIDER) {
                case MapProvider.MAPBOX:
                    layer = new MapboxLayer(layerID, node["name"], source);

                    // Store display order if present
                    if(node.hasOwnProperty("order")) {
                        layer.order = node["order"];
                    }

                    // Register this layer to this connected stack
                    if(!Manager.STACK_LAYERS.hasOwnProperty(stack)) {
                        Manager.STACK_LAYERS[stack] = [];
                    }
                    Manager.STACK_LAYERS[stack].push(layerID);
                break;
    
                case MapProvider.CESIUM:
                    layer = new CesiumLayer(layerID, node["name"], source);

                    // Store display order if present
                    if(node.hasOwnProperty("order")) {
                        layer.order = node["order"];
                    }

                    // Register this layer to this connected stack
                    if(!Manager.STACK_LAYERS.hasOwnProperty(stack)) {
                        Manager.STACK_LAYERS[stack] = [];
                    }
                    Manager.STACK_LAYERS[stack].push(layerID);
                break;

                default:
                    throw new Error("Unknown map provider specified!");
                break;
            }
           
            layer.definition = node;
            this.dataLayers.push(layer);
        }
    }

    private findSource(rawID: string): DataSource {
        let array = [];
        this.recurseFindSource(array, this, rawID);
        return (array.length === 1) ? array[0] : null;
    }

    /**
     * Recurse
     */
    private recurseFindSource(array, currentGroup, target) {
        let source = currentGroup.dataSources.find(source => {
            let parts = source.id.split(".");
            return parts[parts.length - 1] === target;
        });

        if(source === null || source === undefined) {
            if(currentGroup.parentGroup !== null && currentGroup.parentGroup !== undefined) {
                this.recurseFindSource(array, currentGroup.parentGroup, target);
            }
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
     * Recurse
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
