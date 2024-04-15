# Augmented Singapore
This repository contains the instructions, directory structure, and configurations required to deploy the Singapore stack. 

## 1. Preparations
### Knowledge of the stack tools adopted in The World Avatar
Please read through the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) and [Stack Data Uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) to set up your stack accordingly.

At the moment, a working understanding of these two tools will suffice for the deployment of the Singapore stack.

## 2. Deployment Workflow
### Stack manager
Recommended stack name: sg

Important note: Instances where 'sg' is committed - [access-agent.json], [sg.json (manager)], [sg.json (uploader)], [client.properties]
1) Add four secret files in [stack-manager-secrets]
    - geoserver_password
	- postgis_password
	- mapbox_username
	- mapbox_api_key
2) Visualisation settings: 
    - Replace all instances of `localhost` in [index.html], [data.json (MapBox)] and [data.json (Cesium)] with digital ocean IP.
3) To generate dispersion data, two additional API keys are needed, one from https://aisstream.io/ and another one from https://openweathermap.org/ with OneCall enabled. Insert API key from aisstream in [ship-input-agent.json] and API key from openweather in [weather-agent.json].
4) Run the following command in [stack-manager]:
```
./stack.sh start sg
```

### Stack data uploader
1) The full dataset is available on Dropbox https://www.dropbox.com/scl/fo/p48nv63fdw1tmzqo9p2ja/h?rlkey=cgk3svzuj1jxds168qstla0pl&dl=0 (only CARES members can access this link). Important note for buildings dataset: it is stored in a separate folder from the shared folder link, and due to the large size, you may need to upload the data in separate chunks.
2) Modify [sg.json (uploader)] to skip/include any datasets.
3) Run the following command in 
```
./stack.sh start sg
```

### Company data
The "buildings" and "company" datasets need to be present for this step. 
- Execute [company.http] to match building IRIs to factories and data centres. This will add a `building_uuid` column to the tables `data_centres` and `factories`.

### City furniture footprint
Execute [cityfurniture-footprint-height.sql] to add footprint data in the `citydb.cityobject_genericattrib` table. This is necessary because unlike the buildings table, the city_furniture table does not contain the lod0 footprint necessary for visualisation on MapBox.

### Dispersion data
To generate dispersion data, make sure the weather agent and ship input agent are spun up with the correct API keys. Examples of HTTP requests are made available in [HTTP requests for dispersion]. 

1) Execute [start-live-updates.http] to start live ship updates from aisstream.
2) Execute [mbs-live.http] to start scheduled simulations. Important thing to consider is at the time of the first dispersion simulation, there should be ships that are instantiated within the simulation boundary (see explanation of delayMinutes below).
- Parameters:
    1) ewkt - Extended WKT literal for PostGIS
    2) nx - number of x cells
    3) ny - number of y cells
    4) z - height to simulate (multiple values can be provided)
    5) label - Text to show in the visualisation for users to select which simulation to display
    6) delayMinutes - Upon submitting the request, the duration to wait before executing a dispersion simulation, it is also the time to subtract from the current time to run the simulation for. For example, if delayMinutes = 30, and the current time is 1pm, the simulation will be executed at 1pm + 30 min, i.e. 130pm, for a simulation at 1pm (using weather and ship data at 1pm).
    7) intervalMinutes - Interval to execute dispersion calculations.

3) To stop either live updates or scheduled simulations, change the request from POST to DELETE.

### Carpark
Add postgis password to db.password in [client.properties].
Run the following requests:
```
curl -X POST --header "Content-Type: application/json" -d "{\"delay\":\"0\",\"interval\":\"180\",\"timeunit\":\"seconds\"}" http://localhost:3838/carpark-agent/retrieve
```

```
curl -X POST http://localhost:3838/carpark-agent/create
```

### GFA data
Make sure ./gfa_config is populated with data. A copy of the data can be obtained from https://www.dropbox.com/scl/fo/9jl3cmee0h26uhm7zyy0f/h?rlkey=mm2wd0al9zrydqxrybbouxzun&dl=0 (only CARES members have access).

Execute the following request to the GFA agent
```
curl -X POST http://localhost:3838/gfaagent/calculation
```
Check contents of the table citydb.cityobject_genericattrib, there should be rows with attrname=GFA.

### GeoServer layer for buildings
Currently the creation of the layer is not automated through the data uploader because it requires data from different sources (city furniture, heat emissions from companies etc.).

Execute [geoserver_layer.sql] to create the materialised view manually. Edit the SQL view of building_usage layer to
```
select * from usage.buildingusage_geoserver_sg
```

## Generating colourbars for visualisation
It is not necessary to execute the following steps unless the colours need to be editted.
This section documents how the colours for population density and heat emissions were generated. The script [colourbar.py] generates two colourbars in the visualisation:
1) Colourbar for heat emissions
The script generates the corresponding colours for each value range (heat_emissions_colours.txt) as well as the colourbar image (heat_emissions_colorbar.png). The colour codes are then used to edit [data.json (MapBox)] to assign the correct colours for the heat emission values.

2) Colourbar for population density
The script generates the corresponding colours for each value range (population_density_colours.txt) and the colourbar (population_density.png). The colour codes are used in [uk-population-style.sld].
 

[access-agent.json]: ./stack-manager/inputs/config/services/access-agent.json
[sg.json (manager)]: ./stack-manager/inputs/config/sg.json
[sg.json (uploader)]: ./stack-data-uploader/inputs/config/sg.json
[stack-manager-secrets]: ./stack-manager/inputs/secrets/
[index.html]: ./stack-manager/inputs/data/webspace/index.html
[data.json (MapBox)]: ./stack-manager/inputs/data/webspace/data.json
[data.json (Cesium)]: ./stack-manager/inputs/data/webspace/3d/data.json
[stack-manager]: ./stack-manager/
[stack-data-uploader]: ./stack-data-uploader/
[ship-input-agent.json]: ./stack-manager/inputs/config/services/ship-input-agent.json
[weather-agent.json]: ./stack-manager/inputs/config/services/weather-agent.json
[company.http]: <./HTTP_requests/company.http>
[start-live-updates.http]: <./HTTP_requests/start-live-updates.http>
[jurong-live.http]: <./HTTP_requests/jurong-live.http>
[mbs-live.http]: <./HTTP_requests/mbs-live.http>
[dispersion-interactor.json]: ./stack-manager/inputs/config/services/dispersion-interactor.json
[client.properties]: ./carpark_config/client.properties
[cityfurniture-footprint-height.sql]: ./custom_sql_scripts/cityfurniture-footprint-height.sql
[geoserver_layer.sql]: ./custom_sql_scripts/geoserver_layer.sql
[colourbar.py]: ./colorbar_generator/colourbar.py
[uk-population-style.sld]: ./stack-data-uploader/inputs/config/uk-population-style.sld