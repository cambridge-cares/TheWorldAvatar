Add GeoJSON file here from https://overpass-turbo.eu/

The map defaults to Rome, move it to Hong Kong or other desired location.

The following query will need to be entered to select the building data from OSM. 

```/*********************************************

This query is compiled by OSM Buildings.
It returns buildings for currently visible
map area.

Click RUN to run the query.
When done, click EXPORT to save the result.

If you are looking for:

- large areas and faster downloads
- additional data sets
- more file formats
- real 3d models from complex buildings
- consistent building types and attributes

All are available on https://3dbuildings.com/

*********************************************/

[out:json][timeout:30];(
way["building"]({{bbox}});
relation["building"]["type"="multipolygon"]({{bbox}});
);out;>;out qt;
```