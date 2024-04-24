import { DataLayer } from './data-layer';
import { DataSource } from './data-source';

/**
 * This class represents a single source of geographical data within the visualisation.
 */
export class DataGroup {

    /**
     * Public facing name.
     */
    public readonly name: string;

    /**
     * Unique, dynamically generated, ID.
     */
    public readonly id: string;

    /**
     * The stack endpoint that stores the associated data for this data group. Note that the subgroups may have different endpoints.
     */
    public readonly stackEndpoint: string;

    /**
     * Data sources.
     */
    public readonly dataSources: DataSource[] = [];

    /**
     * Data layers.
     */
    public readonly dataLayers: DataLayer[] = [];

    /** 
     * Sub-groups.
     */
    public readonly subGroups: DataGroup[] = [];

    /**
     * Parent group
     */
    public parentGroup: DataGroup;

    /**
     * Should this group be expanded in the layer tree?
     */
    public defaultExpanded: boolean = true; 

    /**
     * Optional icon for display in layer tree.
     */
    public treeIcon: string;

    /**
     * Initialise a new DataGroup object.
     * 
     * @param name public facing name.
     * @param id unique ID.
     * @param stackEndpoint The stack endpoint storing the data.
     */
    constructor(name: string, id: string, stackEndpoint: string) {
        this.name = name;
        this.id = id;
        this.stackEndpoint = stackEndpoint;
    }
    
    /**
     * Returns the first DataSource instance with the input ID if present.
     * 
     * @param targetID target ID data source.
     * 
     * @return matching DataSource, null if not present.
     */
    public getFirstSourceWithID(targetID: string): DataSource {
        const results: DataSource[] = [];

        this.recurseFindSources(
            this,
            results,
            (source: DataSource) => source.id === targetID
        );
        
        return (results.length === 1) ? results[0] : null;
    }

    /**
     * Returns the first DataLayer instance with the input ID if present.
     * 
     * @param targetID target ID data layer.
     * 
     * @return matching DataLayer, null if not present.
     */
    public getFirstLayerWithID(targetID: string): DataLayer {
        const results: DataLayer[] = [];

        this.recurseFindLayers(
            this,
            results,
            (layer: DataLayer) => layer.id === targetID
        );

        return (results.length === 1) ? results[0] : null;
    }

    /**
     * Working from this group down through the tree, collect and
     * return a flat list of all defined DataLayer instances.
     * 
     * @returns all data sources from this group and its children
     */
     public getLayersFlat(): DataLayer[] {
        const results: DataLayer[] = [];

        this.recurseFindLayers(
            this,
            results,
            (layer: DataLayer) => layer.id != null
        );

        return results;
    }

    /**
     * Recurse through the layers within this group, and its sub-groups, and returns any
     * layer that meets the matching function.
     * 
     * @param currentGroup data group being searched.
     * @param results running pool of matching layers.
     * @param matchingFunction function that takes a single DataLayer argument and returns a boolean,
     * matches are added to the results array.
     */
    private recurseFindLayers(
        currentGroup: DataGroup,
        results: DataLayer[],
        matchingFunction: (layer: DataLayer) => boolean) {
        
        // Get matching layers in this group
        const layer: DataLayer = currentGroup.dataLayers.find(layer => matchingFunction(layer));
        if(layer != null) results.push(layer);

        // Recurse deeper
        currentGroup.subGroups.forEach(subGroup => {
            this.recurseFindLayers(subGroup, results, matchingFunction);
        });
    }

    /**
     * Recurse through the sources within this group, and its sub-groups, and returns any
     * source that meets the matching function.
     * 
     * @param currentGroup data group being searched.
     * @param results running pool of matching sources.
     * @param matchingFunction function that takes a single DataSource argument and returns a boolean,
     * matches are added to the results array.
     */
    private recurseFindSources(
        currentGroup: DataGroup,
        results: DataSource[],
        matchingFunction: (source: DataSource) => boolean) {
        
        // Get matching source in this group
        const source: DataSource = currentGroup.dataSources.find(source => matchingFunction(source));
        if(source != null) results.push(source);

        // Recurse deeper
        currentGroup.subGroups.forEach(subGroup => {
            this.recurseFindSources(subGroup, results, matchingFunction);
        });
    }
}
