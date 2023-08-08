# Kings Lynn Flood Routing Digital Twin Visualisation Framework (DTVF)

This visualization serves as a proof of concept for routing under flood, isochrone from points of interest, unreachable area.

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]) version `3.3.4`. The configuration file structure (i.e. `data.json`) is based on the [example Mapbox visualisation].

<img src="floodrouter.JPG" alt="Mapbox visualisation" width="100%"/>

## Creating the Visualisation
This document marks down the steps taken to create flood router, isochrone under flooding and transport network criticality analysis. 

## Pre-requisite for setting up.
Before spinning up the stack insert [geoserver.json](KingsLynn_vis\inputs\geoserver) into services folder of stack-manager. This steps allows further modifcation and publishing of geoserver layer.

Spin up the stack-manager with `kingslynn` as the `<STACK-NAME>` by running `./stack.sh start kingslynn`

## Geoserver layers
Geoserver setting configurations: 
1) Under Settings - Globals, change the number of decimals to 6. 

For each layers published: 
1) In lat/lon bounding box, select compute for data and compute from native bounds.
2) Under tile caching tab, enable application/json;type=geojson, application/vnd.mapbox-vector-tile under Tile Image Formats.

### Publishing Geoserver layers
Under layers tab, add a new layer, select kingslynn:routing_ways, publish the following layers: 
1) isochrone_hospital_unflooded
2) isochrone_hospital_flooded
3) unreachable_population
4) flood_polygon_single

### Configure new SQL view
Under layers tab, add a new layer, select kingslynn:routing_ways, configure new SQL views geoserver as below: 
1) tsp_visualization
2) tsp_nodes 
3) routing_pointofinterest (edit the SQL view). routing_pointofinterest is supposed to be shortest_paths, this is just a quick fudge fix. 

Use SQL commands found under [virtualTables](inputs/data/kingslynn/sql/virtualTables).

Subsequently, you may run the DTVF container.

### DTVF Prerequisite
A valid Mapbox API token must be provided in your [index.html] file.

```
# To build the Image:
docker-compose -f ./docker/docker-compose.yml build --force-rm

# To generate a Container (i.e. run the Image):
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate
```

Visualization can be seen at [http://localhost:80](http://localhost:80)


<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent subdirectory]: /DTVF/FeatureInfoAgent
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[DTVF subdirectory]: /DTVF
[icons]: /DTVF/data/icons
[index.html]: webspace/index.html
[data.json]: /DTVF/data.json