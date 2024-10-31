# Kaiserslautern

This is a collection of stack input configuration files to spin up a basic visualisation of some data of the city of Kaiserslautern.

## Start the stack

Make sure to populate ./stack-manager-inputs/inputs/secrets/ with these files

geoserver_password & postgis_password

start the stack with the following command:

```
./stack.sh start <STACK_NAME>
```

NOTE: geoserver_password and postgis_password are personal choices, valid credential for MapBox is required to view the visualisation.

For more information: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager

## Data Uploader

The district boundary dataset offers detailed boundary measurements for Kaiserslautern, while the second dataset provides information on various parking zones and parking garages within the city. Third is the 3D building models dataset, it describes three-dimensional building and structure information based on the floor plans of the real estate cadastre in various levels of detail (LoD). The LoD2 describes all buildings, structures or parts of buildings with standardized roof shapes, It is a GML file. Plots tells us about the ID, Owner, Zone, Price, the status of the plot, Previous Use of the Plot and the contamination type that exists in the plot to determine if it is a Brownfield site or a normal site. The shapefiles are created using QGIS Software. Tbox for the Kaiserslautern is created by reusing the Ontoplot, Ontozoning and OntobuiltEnv. 

Contents for the stack data uploader can be found at :
1. https://opendata.kaiserslautern.de/dataset/ortsbezirksgrenzen-kaiserslautern for district boundary dataset.

2. https://opendata.kaiserslautern.de/dataset/parken-in-kaiserslautern for parking zones and parking garages.

3. Miro board link for the ontologies: https://miro.com/welcomeonboard/blBpUm9ENnhjQWZjaGIxT3RudmY4QzR4dE9IVzd3MGZlUzl4Q0w2Mm5zUnc4YlNTbldteG1IRmJaSzc5MGlDQnwzNDU4NzY0NTg4MTg3MjIxNzEyfDI=?share_link_id=389009059663 

Place the data file in ./stack-data-uploader-inputs/data

Place config files in ./stack-data-uploader-inputs/config


start the data uploader with the following command:

```
./stack.sh start <STACK_NAME>
```
After running the command verify if Geoserver, PostgreSQL and Blazegraph works accurately.  

## Visualisation

Visualisation will be available at http://localhost:3838/visualisation locally. 

Make sure to have mapbox_api_key and mapbox_username in the `TWA-VIS-PLATFORM` folder.


Navigate to the `Web->TWA-VIS-PLATFORM` directory. Ensure that the `CONFIG` folder contains the four JSON files: `data-settings.json`, `data.json`, `map-settings.json`, and `ui-settings.json`. Changes are made only in the `data.json` file, Copy the GeoJSON link from GeoServer and paste the link in the "data" attribute of the  `data.json` file. These visualisation files should be in the `Web->TWA-VIS-PLATFORM->CODE->PUBLIC->CONFIG`.

Additionally, verify that the `OPTIONAL-PAGES` folder includes a `landing-page.md` file. Lastly, update the `docker-compose.yml` file by setting `KEYCLOAK` to `false`.
Run the Docker Deployment command and verify the layers.

For more information: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform
