# Producing a .geoJSON file from a triple-store

###  _Author - Tom Savage, March 2021_

All of the python scripts in this folder work in more or less the same way. 

1) Query a triple-store for the locations and attributes of some entity.
2) Parse this information into a table. 
3) Convert the information in this table to a .geoJSON file based on the standard geoJSON format.

This geoJSON file can then be displayed in a number of tools including: 
* [Mapbox](https://www.mapbox.com)
* [Leaflet](https://leafletjs.com)
* [Folium](https://python-visualization.github.io/folium/) 
* www.geojson.io 

### Note
A key aspect of this code is that attributes of the locations (for example power station capacities) can be called from the triple store along with the locations. This information can then be encoded within the geoJSON file within the 'properties:' key. Therefore information can be displayed on a map without additional calls to the triple store. 
---
### Potential Errors 
If you're hosting the geoJSON file locally and plan to run the HTML locally, you'll need to do so with a local server, otherwise the .geoJSON file wont appear on the map. 
This can be done with the command:
'''
python -m http.server 
'''
'''
python3 -m http.server 
'''
Or alternativly in VSCode, using the 'Live Server' extension 