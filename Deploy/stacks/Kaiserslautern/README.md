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

The district boundary dataset offers detailed boundary measurements for Kaiserslautern, while the second dataset provides information on various parking zones and parking garages within the city. All three data files are available in GeoJSON format. 

Contents for the stack data uploader can be found at :
1. https://opendata.kaiserslautern.de/dataset/ortsbezirksgrenzen-kaiserslautern for district boundary dataset.

2. https://opendata.kaiserslautern.de/dataset/parken-in-kaiserslautern for parking zones and parking garages.



Place the data file in ./stack-data-uploader-inputs/data

Place config files in ./stack-data-uploader-inputs/config


start the data uploader with the following command:

```
./stack.sh start <STACK_NAME>
```

## Visualisation

Visualisation will be available at http://localhost:3838/visualisation locally. 

Make sure to have mapbox_api_key and mapbox_username in the `TWA-VIS-PLATFORM` folder.


Navigate to the `Web->TWA-VIS-PLATFORM` directory. Ensure that the `CONFIG` folder contains the four JSON files: `data-settings.json`, `data.json`, `map-settings.json`, and `ui-settings.json`. Changes are made only in the `data.json` file, Copy the GeoJSON link from GeoServer and paste the link in the "data" attribute of the  `data.json` file. These visualisation files should be in the `Web->TWA-VIS-PLATFORM->CODE->PUBLIC->CONFIG`.

Additionally, verify that the `OPTIONAL-PAGES` folder includes a `landing-page.md` file. Lastly, update the `docker-compose.yml` file by setting `KEYCLOAK` to `false`.
Run the Docker Deployment command and verify the layers.

For more information: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform
