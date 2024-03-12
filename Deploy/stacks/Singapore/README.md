# Augmented Singapore
This repository contains the instructions, directory structure, and configurations required to deploy the Singapore stack. 

## 1. Preparations
### Knowledge of the stack tools adopted in The World Avatar
Please read through the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) and [Stack Data Uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) to set up your stack accordingly.

At the moment, a working understanding of these two tools will suffice for the deployment of the Singapore stack.

## 2. Deployment Workflow
### Stack manager
Recommended stack name: sg

Important note: Instances where 'sg' is committed - [access-agent.json], [sg.json (manager)], [sg.json (uploader)]
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
1) The full dataset is available on Dropbox https://www.dropbox.com/scl/fo/j4pry0134ewsqzzk88hg8/h?rlkey=at1l7s7jz62fjsg7qao62vcbs&dl=0 (only CARES members can access this link). Important note for buildings dataset: it is stored in a separate folder from the shared folder link, and due to the large size, you may need to upload the data in separate chunks.
2) Modify [sg.json (uploader)] to skip/include any datasets.
3) Run the following command in 
```
./stack.sh start sg
```

### Company data
The "buildings" and "company" datasets need to be uploaded for this step. 
1) Execute [company.http] to match building IRIs to respective companies, this will add a column in the company table:
2) Once the building IRIs have been appended, please go to the `mainland` geoserver layer and update the query as below:
    ```
    WITH "uuid_table" AS 
    ( SELECT "strval" AS "uuid", "cityobject_id" FROM "citydb"."cityobject_genericattrib" WHERE "attrname" = 'uuid' ), 
    "iri_table" AS ( SELECT "urival" AS "iri", "cityobject_id" FROM "citydb"."cityobject_genericattrib" WHERE "attrname" = 'iri' ), 
    // new line below
    "companies" AS (SELECT "heat_emissions", "building_iri" FROM "company") 
    SELECT "building"."id" AS "building_id", 
    COALESCE("measured_height", 100.0) AS "building_height", 
    "geometry", 
    "uuid", 
    "iri" ,
    "heat_emissions" // new parameter
    FROM "citydb"."building" 
    JOIN "citydb"."surface_geometry" ON "citydb"."surface_geometry"."root_id" = "citydb"."building"."lod0_footprint_id" 
    JOIN "uuid_table" ON "citydb"."building"."id" = "uuid_table"."cityobject_id" 
    JOIN "iri_table" ON "citydb"."building"."id" = "iri_table"."cityobject_id"
    LEFT JOIN "companies" ON "uuid_table"."uuid" = "companies"."building_iri" // new line
    WHERE "citydb"."surface_geometry"."geometry" IS NOT NULL
    ```

### Dispersion data
To generate dispersion data, make sure the weather agent and ship input agent are spun up with the correct API keys. Examples of HTTP requests are made available in [HTTP requests for dispersion]. 

1) Execute [start-live-updates.http] to start live ship updates from aisstream.
2) Execute [jurong-live.http] and [mbs-live.http] to start scheduled simulations. Important thing to consider is at the time of the first dispersion simulation, there should be ships that are instantiated within the simulation boundary (see explanation of delayMinutes below).
- Parameters:
    1) ewkt - Extended WKT literal for PostGIS
    2) nx - number of x cells
    3) ny - number of y cells
    4) z - height to simulate (multiple values can be provided)
    5) label - Text to show in the visualisation for users to select which simulation to display
    6) delayMinutes - Upon submitting the request, the duration to wait before executing a dispersion simulation, it is also the time to subtract from the current time to run the simulation for. For example, if delayMinutes = 30, and the current time is 1pm, the simulation will be executed at 1pm + 30 min, i.e. 130pm, for a simulation at 1pm (using weather and ship data at 1pm).
    7) intervalMinutes - Interval to execute dispersion calculations.

3) To stop either live updates or scheduled simulations, change the request from POST to DELETE.

## 3. Miscellaneous Functions
### Legend
The mapbox visualisation can currently generate legends for different parameters manually. Please  check out the `manager.getPanelHandler().setLegend(htmlContent);` line at the `./stack-manager/inputs/data/webspace/index.html`.
New sets of gradient bars can be generated in the `./stack-manager/inputs/data/webspace/component/legend.css`.

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
[company.http]: <./HTTP requests for dispersion/company.http>
[start-live-updates.http]: <./HTTP requests for dispersion/start-live-updates.http>
[jurong-live.http]: <./HTTP requests for dispersion/jurong-live.http>
[mbs-live.http]: <./HTTP requests for dispersion/mbs-live.http>
[dispersion-interactor.json]: ./stack-manager/inputs/config/services/dispersion-interactor.json