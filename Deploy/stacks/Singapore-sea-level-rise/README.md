# Singapore Sea-Level Rise
This repository contains the instructions, directory structure and configurations required to deploy Singapore stack for Sea-Level-Rise analysis which builds on top of the existing [Augmented Singapore](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-singapore-stack/Deploy/stacks/Singapore). 

## Data
Comprehensive data collated can be found in the [CARES dropbox link](https://www.dropbox.com/scl/fo/s4youc2epx7quqapolgw6/AH_IAMDhH9FppOosYpKd3zs?rlkey=4ab335m057bkv64zs7e8xdn20&dl=0). 

### Cultural Sites and Trees
Cultural sites - Historic Sites, Monuments, Museums, Tourist Attractions
Trees - Trees, Heritage trees 

Cultural Sites and Trees data are retrieved from [Singapore's Open Data Portal](https://beta.data.gov.sg/).

### Digital Elevation Model
SRTM Digital Elevation Model 2000 are retrieved from [USGS EROS Archive - Digital Elevation](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1#overview).

### Landplots
Landplots are retrieved from [Singapore's Open Data Portal](https://beta.data.gov.sg/)'s URA Master Plan 2019.

### Population
Singapore population distribution 2000 is retrieved from [Facebook Data For Good](https://dataforgood.facebook.com/dfg/tools/high-resolution-population-density-maps).

### Street network data
Street network data is retrieved from OpenStreetMap through [Geofabrik](https://download.geofabrik.de/).

### Sea Level Rise
Sea level rise projections are provided by the courtesy of Prof. Benjamin Horton and Dr. Timothy A. Shaw from Nanyang Technological University, Earth Observatory of Singapore, published under [`Shaw, T.A., Li, T., Ng, T. et al. Deglacial perspectives of future sea level for Singapore. Commun Earth Environ 4, 204 (2023)`](https://doi.org/10.1038/s43247-023-00868-5).

## Agent Setup
### OSMAgent
[OSMAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OSMAgent) is run to match OSM entities with Singapore's CityGML buildings. For remaining buildings not matched to any OSM entities, OSMAgent will match with the underlying landplot usage. 

### BuildingFloorAgent
[BuildingFloorAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BuildingFloorAgent) derives number of floors for 3D Buildings using three methods 
- HDB property Information
- OpenStreetMap
- Building floors from building height

The purpose of this agent is to augment buildings with more accurate data on the number of floors for the subsequent calculcations of GFA and construction cost. To run this agent: 
- Prerequisite: OSM data and ontop mapping added by OSM agent
- Download [HDBPropertyInformation.csv](https://www.dropbox.com/scl/fi/3pgkir5zfcbhq8dliv1kr/HDBPropertyInformation.csv?rlkey=5lmb49cjqgvyrx7rcxtos1l41&dl=0) and place it in ```./buildingfloordata```
- Modify [sea-level.json] to include this agent within services, the following example also includes gfaagent for the next step
```json
{
    "services": {
        "includes": [
            "visualisation",
            "feature-info-agent",
            "filter-agent",
            "sealevelimpactagent",
            "buildingflooragent", // modified
            "gfaagent" // modified 
        ],
```
- Restart stack-manager to spin up this agent
- Execute 
```
curl -X POST http://localhost:3838/buildingflooragent/
```
- Check contents of ```gfa_floors.floors```, the number of rows should equate the number of buildings
### GFAAgent
[GFAAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-sea-level-rise-singapore/Agents/GFAAgent) computes the Gross Floor Area (GFA) and the construction cost of buildings. 

- Prequisites: 
    1) Floors data added by BuildingFloorAgent
    2) Building geometries in citydb
    3) OSM usage information added by OSM agent
- Modify [sea-level.json] to include this agent within services (not needed if already spun up)
```json
{
    "services": {
        "includes": [
            "visualisation",
            "feature-info-agent",
            "filter-agent",
            "sealevelimpactagent",
            "buildingflooragent", // modified
            "gfaagent" // modified 
        ],
```
- Execute the following
```
curl -X POST http://localhost:3838/gfaagent/gfa
```
- Make sure the table `gfa_floors.gfa` is populated with data
- Then execute
```
curl -X POST http://localhost:3838/gfaagent/cost
```

### SeaLevelImpactAgent
[SeaLevelImpactAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/SeaLevelImpactAgent) assess the affected cultural sites, buildings, road networks and landplot, given a sea level rise projections.

## Deployment 
### Starting the stack-manager
To deploy the Singapore's Sea Level Rise tech stack, a default stack-name `sea-level` is pre-configured.

In the [stack-manager](stack-manager) directory, run the command below to spin up the tech stack,
```
./stack.sh start sea-level
```

### Uploading data
Once the stack-manager is fully spin up. In the [stack-data-uploader](stack-data-uploader) directory, replace the content of the directory with data from [CARES dropbox link](https://www.dropbox.com/scl/fo/s4youc2epx7quqapolgw6/AH_IAMDhH9FppOosYpKd3zs?rlkey=4ab335m057bkv64zs7e8xdn20&dl=0), run the command to upload data:
```
./stack.sh start sea-level
```

### Landplot layer
Execute [landplot_matching.http] to match buildings with landplot. This should create a table public.landplot_buildings, mapping ogc_fid of landplots to building_uuid. Required to highlight buildings with GFA exceeded.

Execute [landplot_layer.sql] to create a new materialised view, and update SQL view of twa:landplot in GeoServer GUI manually to
```
SELECT * FROM landplot_layer
```
Manually add this ontop mapping to the ontop container, prefixes are dependent on current state of the ontop mapping:
```
mappingId	landplot to buildings
target		tw:landplot/{ogc_fid} tw:ontoplot/containsBuilding tw:Building/{building_uuid} . 
source		SELECT ogc_fid, building_uuid from landplot_buildings
```

### Carpark
Create carpark namespace in Blazegraph and carpark database in PostGIS.
Add postgis password to db.password in [client.properties] and carpark API token in ./carpark_config/api.properties.
Run the following requests from the command line or use the convenience file [carpark.http]:
```
curl -X POST --header "Content-Type: application/json" -d "{\"delay\":\"0\",\"interval\":\"180\",\"timeunit\":\"seconds\"}" http://localhost:3838/carpark-agent/retrieve
```

```
curl -X POST http://localhost:3838/carpark-agent/create
```

### City furniture
Execute [cityfurniture-footprint-height.sql] first to add city furniture footprint into the cityobject_genericattrib table.

### Company
Then run the data uploader for the "company" and "" dataset, which is not in the list of datasets in sea-level.json.

Not run together with data uploader due to the SQL script requiring the city furniture footprint first.

Finally run [company.http] to match entities to the closest buildings.

### CEA data
CEA data is stored in the `cea` namespace in Blazegraph and the `CEAAgent` database in postgres for timeseries data.

The CEA agent has not been run directly on this stack. Refer to [CEAAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CEAAgent) for more details, it requires [AccessAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent) and [OpenMeteoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OpenMeteoAgent). Namespace for OpenMeteoAgent requires Blazegraph's geospatial capabilities to be enabled.

In the visualisation, there is a layer that shows buildings with CEA data, a table containing the IRIs of buildings with CEA data is required for the layer. This table is named `cea`, located in the default `postgres` database, under the `cea` schema.

The contents of this table is generated via the following query on the `cea` namespace:
```
SELECT ?s WHERE {?s a <http://www.opengis.net/citygml/building/2.0/Building> .}
```

### Update buildings layer
Previously the stack data uploader executed a script to create the GeoServer layer for buildings, but it does not have city furniture and company data. Execute [geoserver_layer.sql] to update the layer.

After updating the layer, the custom cea layer needs to be created manually as twa:cea in GeoServer, with the following SQL view
```
SELECT * FROM buildings_with_cea
```

### CARES weather station
Create `caresweather` namespace in Blazegraph. 

Modify [api.properties (weather)] with credentials from https://www.dropbox.com/scl/fo/24gyly40ezoyx04i6xomi/AHLZ6DHl_IVFNXy6Ym2-oFc?rlkey=xpvzka6b5smg53cppe3f98zt8&st=mleraqpx&dl=0

Update db.password in [client.properties (weather)].

Time series data for CARES weather station is stored in the main database (postgres) due to it sharing the same rdf type with virtual weather station and virtual sensors.

Execute [cares_weather.http] to instantiate and start periodic updates.

### nginx for PostGIS
The stack's PostGIS is accessible at port 3840, to change the port edit [nginx-2].

### Traffic incident Agent
You need to have an `API_key` which can be obtained by registering from [Land Transport Data Mall](https://datamall.lta.gov.sg/content/datamall/en/request-for-api.html). The API key needs to entered in the file [config.properties](./traffic_incident_inputs/config.properties) at `trafficincident.accountKey=` to retrieve the data from LTA API.

To trigger the agent, run
```bash
curl -L -X POST "http://localhost:3838/traffic-incident-agent/start"
```
Refer [TrafficIncidentAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/TrafficIncidentAgent) for more details.

## Authors
Shin Zert Phua (shinzert.phua@cares.cam.ac.uk), May 2024
Kok Foong Lee (kokfoong.lee@cares.cam.ac.uk)

[sea-level.json]: ./stack-manager/inputs/config/sea-level.json
[landplot_matching.http]: ./http_requests/landplot_matching.http
[landplot_layer.sql]: ./additional_sql_scripts/landplot_layer.sql
[cityfurniture-footprint-height.sql]: ./additional_sql_scripts/cityfurniture-footprint-height.sql
[company.http]: ./http_requests/company.http
[geoserver_layer.sql]: ./additional_sql_scripts/geoserver_layer.sql
[api.properties (weather)]: ./cares_weather_config/api.properties
[client.properties (weather)]: ./cares_weather_config/client.properties
[cares_weather.http]: ./http_requests/cares_weather.http
[nginx-2]: ./stack-manager/inputs/config/services/nginx-2.json