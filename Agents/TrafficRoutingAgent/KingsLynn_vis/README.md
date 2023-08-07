# Kings Lynn Flood Routing Digital Twin Visualisation Framework (DTVF)

This visualization serves as a proof of concept for routing under flood, isochrone from points of interest, unreachable area.

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]) version `3.3.4`. The configuration file structure (i.e. `data.json`) is based on the [example Mapbox visualisation].

<img src="floodrouter.JPG" alt="Mapbox visualisation" width="100%"/>

## Creating the Visualisation
This document marks down the steps taken to create flood router, isochrone under flooding and transport network criticality analysis. 

To run this agent, use `kingslynn` as the STACK-NAME, before spinning up the stack insert geoserver.json into services folder of stack-manager, retrieve the geoserver.json from `inputs\geoserver`. This steps allows further modifcation and publishing of geoserver layer. 

## Geoserver layers
Note:
1) For lat/lon bounding box, compute for data and compute from native bounds
2) Under tile caching tab, enable application/json;type=geojson,  application/vnd.mapbox-vector-tile under Tile Image Formats

### Publishing Geoserver layers
Publish the following layers: 
1) isochrone_hospital_unflooded
2) isochrone_hospital_flooded
3) unreachable_population
4) flood_polygon_single

### Configure new SQL view
Create new SQL view layers in geoserver as below: 
1) tsp_visualization
2) tsp_nodes

Use SQL commands found under `inputs\data\kingslynn\sql\virtualTables`.

Subsequently, your settings is properly set up and you may run the DTVF visualization to visualize the floodrouter.

### DTVF Prerequisite
A valid Mapbox API token must be provided in your `index.html` file.

```
# To build the Image:
docker-compose -f ./docker/docker-compose.yml build --force-rm

# To generate a Container (i.e. run the Image):
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate
```

<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent subdirectory]: /DTVF/FeatureInfoAgent
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[DTVF subdirectory]: /DTVF
[icons]: /DTVF/data/icons
[index.html]: index.html
[data.json]: /DTVF/data.json