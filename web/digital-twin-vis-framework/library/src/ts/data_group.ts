/**
 * This class represents a single source of geographical data within the visualisation.
 */
class DataGroup {

    public dataSources: Array<DataSource> = [];

    public subGroups: Array<DataGroup> = [];

    public name: String;

    public location: String;

    public subLabel: String;

    public groupMeta: Object;
    
    
    constructor(name: String, location: String) {
        this.name = name;
        this.location = location;
    }

    flatten() {
        let flatSources = [];
        this.recurseFlatten(flatSources, this);
        return flatSources;
    }
    
    recurseFlatten(array, currentGroup) {
        if (currentGroup !== null) {
            currentGroup.dataSources.forEach((dataSource) => {
                array.push(dataSource);
            });
        }
        currentGroup.subGroups.forEach((subGroup) => {
            this.recurseFlatten(array, subGroup);
        });
    }
}
