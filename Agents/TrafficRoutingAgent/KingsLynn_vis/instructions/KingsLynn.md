# Kings Lynn Flood Router

This document marks down the steps taken to create flood router, isochrone under flooding and transport network criticality analysis. 

To run this agent, use `kingslynn` as the STACK-NAME, before spinning up the stack insert geoserver.json into services folder of stack-manager, retrieve the geoserver.json from `inputs\geoserver`. This steps allows further modifcation and publishing of geoserver layer. 

# Geoserver layers
Note:
1) For lat/lon bounding box, compute for data and compute from native bounds
2) Under tile caching tab, enable application/json;type=geojson,  application/vnd.mapbox-vector-tile under Tile Image Formats

## Publishing Geoserver layers
Publish the following layers: 
1) isochrone_hospital_unflooded
2) isochrone_hospital_flooded
3) unreachable_population
4) flood_polygon_single

## Configure new SQL view
Create new SQL view layers in geoserver as below: 
1) tsp_visualization
2) tsp_nodes

Use SQL commands found under `inputs\data\kingslynn\sql\virtualTables`.

Subsequently, your settings is properly set up and you may run the DTVF visualization to visualize the floodrouter.