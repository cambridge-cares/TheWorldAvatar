# King's Lynn Stack

This project contains a step-by-step guide on how to spin up the Docker stack (developed by CMCL) for the King's Lynn use case and instantiate all relevant data. It links to other projects and helper scripts where appropriate.

Key sections:
- [1. Prerequisites](#1-prerequisites): Preparations required before spinning up the use case stack
- [2. Spinning up the Stack](#2-spinning-up-the-stack): How to spin up the core stack and upload initial data sets
- [3. Data instantiation workflow](#3-data-instantiation-workflow): How to deploy all required agents (sequence, interdependencies, etc.)
- [5. Triggering new derivation cascades](#5-triggering-new-derivation-cascades): How to manually trigger new derivation cascades (mainly for showcase purposes)
- [6. Redeployment](#6-redeployment): How to restart stack and agents (after initial data instantiation workflow)
- [7. Incorporate CReDo network visualisation](#7-connecting-with-credo-visualisation): How to incorporate synthetic network data from CReDo into the visualisation
- [Potential refinements/next steps](#potential-refinementsnext-steps): Potential refinements for future work


&nbsp;
# 1. Prerequisites

## Access to Docker registries

Spinning up the Docker stack requires access to the [Container registry on Github] to pull (agent) images. Access needs to be ensured beforehand via your Github [personal access token], which must have a `scope` that [allows you to publish and install packages].

To log in to the container registry, please run the following command to establish the connections and provide your access token when prompted. For more details please refer to the linked resources.
```bash
# Github Container registry
$ docker login ghcr.io -u <github_username>
$ <github_personal_access_token>
```

&nbsp;
# 2. Spinning up the Stack

This section explains how to spin up the core stack and upload initial data sets, i.e. high-resolution population raster data and pre-instantiated OntoCityGml building triples.
If using VSCode, all required VSCode extensions shall be installed (on the remote machine if applicable) for all convenience scripts to work properly, i.e. *augustocdias.tasks-shell-input*.


## Spinning up the core Stack

Before spinning up the stack using the [Stack manager], please provide the following files to the specified repositories:

-  4 secret files in `./inputs/stack-manager/inputs/secrets`:
    - `postgis_password`
    - `geoserver_password`
    - `mapbox_username`
    - `mapbox_api_key`
-  Download the geojson as specified in `./inputs/stack-manager/inputs/data/visualisation/data/buildings/readme.txt`
- Create missing colourbars using the provided utility scripts in `Utilities/dtvf_legends` (this will create 3 additional legend figures in `./inputs/stack-manager/inputs/data/visualisation/data/icons`)

Subsequently, copy all files in `./inputs/stack-manager/` into their corresponding repositories of the stack-manager, i.e., under `Deploy/stacks/dynamic/stack-manager/`.

Then navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start KINGS-LYNN
# Stop the stack
bash ./stack.sh stop KINGS-LYNN
# Remove the stack (incl. volumes)
bash ./stack.sh remove KINGS-LYNN -v
# Remove individual service
bash ./stack.sh remove KINGS-LYNN <service name>
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [Stack manager] README.

After stack startup, please ensure that Geoserver supports serving `MapBox Vector Tiles` (as required by the TWF Visualisation Framework). In case this option does not yet appear in the Geoserver GUI, please restart the Geoserver service (It seems that the plug-in download URL required to enable `MapBox Vector Tiles` works but might have intermittent issues.)

## Spinning up the core Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start deployment. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout <BRANCH NAME>
$ git pull
```
Once the repository clone is obtained, please follow the instructions above to spin up the stack on the remote machine. In order to access the exposed endpoints, e.g. `http://<host IP>:3838/blazegraph/ui`, please make sure that the respective ports are open on the remove machine. 

Some issues have been encountered in interacting with the GeoServer GUI when running remotely (e.g. inability to remove layers, edit CRS information, etc.). In such cases, please try [forwarding the port] used by the stack to your local machine after having established the SSH tunnel (e.g. via VSCode for remote development). This ensures that Geoserver becomes available at `http://localhost:3838/geoserver/` as compared to e.g. `http://165.232.172.16:3838/geoserver/`, which seems to solve most of the issues encountered.

To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

```bash
# Check permissions
ls -l <REPO NAME>
# Grant permissions
chmod -R +rwx <REPO NAME>
```

## Uploading initial data

A few datasets and files which shall initially be uploaded to the stack are provided in the `./inputs/stack-data-uploader` folder of this repository. Uploading pre-instantiated OntoCityGml quads is optional but highly recommended to skip steps 1.0 - 1.6 (depending on the exact quads file provided) of the building instantiation workflow below.

The following steps explain how to upload the data to the stack using the [Stack data uploader] (please see the referenced README for more details):

1) Copy all relevant files from the `./inputs/stack-data-uploader` folder of this repository into the `inputs` folder of  the stack data uploader repository:

    a) Copy the configuration files from the `./inputs/stack-data-uploader/config/` directory to the matching directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/config/`

    b) Replace the `readme.txt` files in the `./inputs/stack-data-uploader/data/*` sub-folders with the referenced data files

    c) Copy all data sub-directories from the `./inputs/stack-data-uploader/data` directory into the matching parent directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/`

2) Navigate to `Deploy/stacks/dynamic/stack-data-uploader` and run the following command there from a *bash* terminal and wait until container has stopped again (i.e. the upload has finished). Specified Blazegraph/Geoserver/... namespaces will be created automatically by the uploader if not already exist.
    ```bash
    bash ./stack.sh start KINGS-LYNN
    ```

&nbsp;
# 3. Data instantiation workflow

The following provides an overview of all steps and agents required to instantiate the data into the KG. Copies of the (potentially) required `docker-compose.yml` files are provided in the [Agent docker-compose file folder] for convenience and reproducibility.


## 1) Building instantiation (OntoCityGml)

Some of the following steps refer to an associated `DATA` repository. In case access is needed, please get in touch with mh807@cam.ac.uk.

## 1.1) Geospatial data consolidation (QGIS, *manual*)

QGIS is used to consolidate various geospatial data sets from Digimap (both Ordnance Survey (OS) Open data as well as Premium OS data are used, i.e. Building Height Attribute and Digital Terrain Model 5m ) into a single shapefile containing all relevant building information. The exact workflow is described in the `QGIS workflow.pptx` in the `DATA/01 QGIS` repository, which also contains the QGIS project file. The output shapefile forms the input for the FME workflow below and can be found under `DATA/02 FME/KingsLynn_cleaned ALL buildings_adjusted building heights_incl UPRNs_final.shp`.

## 1.2) Creation of .gml input file for KG import (FME, *manual*)

FME is used to convert the shapefile from the previous step into a `.gml` file that can be instantiated into the KG using the Import Agent/Importer Tool. The exact FME workflow is provided by `shapefile2citygml Kings Lynn BHA data_final.fmw` in the `DATA/02 FME` repository, which also contains both its input and output file. The output `.gml` file contains (all) buildings in King's Lynn in LOD1 including their height (i.e. both building ground elevation as well as actual building height (is both premium data)) and UPRN information.

## 1.3) Access Agent

All CitiesKG Agents require the [AccessAgent] to be running (locally) as Docker container in order to access the target KG namespaces. Details on how to deploy the AccessAgent and upload required routing information are detailed in the [AccessAgent] README and summarised below:

1) Navigate to `Agents/AccessAgent/access-agent-dev-stack` on TWAs `main` branch
2) Pull Docker image and start container by running `docker-compose up -d --no-build`. The agent shall start at port `48888`
3) Replace initial `routing.json` within AccessAgent repository with provided [routing.json] file
4) Upload routing information by running `bash ./uploadRouting.sh`

**Please note** that the `uri.route` within the [CKG config.properties] file needs to match the label used in the [routing.json] file, i.e. `uri.route=http://localhost:48888/ocgml_buildings`. After providing the correct `uri.route`, the City Agents `.war` file can be built and deployed as described in the [CityImportAgent] README.

## 1.4) Importing building data into KG (CitiesKG Importer, *partially manual*)

> The following steps refer to commit `7c378e97d268b02e0d70661257894d5bff8e3655` on `https://github.com/cambridge-cares/CitiesKG/tree/develop`

The [CityImportAgent] can be used to import the `.gml` file from the previous step into the KG. However, the version at time of writing faces issues with larger `.gml` files and a manual workaround is required as detailed below. Please note that Java 8 and IntelliJ are required to build and run the CityImportAgent. Furthermore, the [AccessAgent] needs to be running locally (as Docker container) in order to access the target KG namespace. The folder `DATA/03 OntoCityGml Instantiation/Standalone_CitiesKG_Blazegraph/` contains a `Start_Blazegraph_with_default_settings.bat` file to bring up a Blazegraph instance with required settings for importing OntoCityGml buildings, which will start at `http://127.0.0.1:9999/blazegraph/`.

It is **not** recommended to re-do step 1.4) and instead use the pre-instantiated OntoCityGml quads provided in the `DATA/99 KG snapshots/1_instantiated_ontocitygml/` repository.

## 1.4.1) City Import Agent

Build and deploy the City Import Agent as described in the [CityImportAgent] README. Required IntelliJ run configurations are provided in the `DATA/03 OntoCityGml Instantiation/IntelliJ RunConfigurations/` repository, which also provides a short step-by-step guide `Building Instantiation_short.pptx`.

The used version at commit `7c378e97d268b02e0d70661257894d5bff8e3655` seems unable to handle large `.gml` files. Hence, the CityImportAgent is primarily used to split the large `.gml` file into multiple smaller `.gml` files to be manually uploaded by the Import GUI as described in the next step.

## 1.4.2) Import GUI

After the `.gml` file is split into smaller files, they can be manually uploaded in chunks of 100-200 files via the Importer GUI. The `Building Instantiation_short.pptx` guide contains a step-by-step description on how to achieve this with required IntelliJ run configurations also provided in the `DATA/03 OntoCityGml Instantiation/IntelliJ RunConfigurations/` repository.

## 1.5) Thematic Surface Discovery Agent (CitiesKG)

> The following steps refer to commit `7c378e97d268b02e0d70661257894d5bff8e3655` on `https://github.com/cambridge-cares/CitiesKG/tree/develop` (however, they should also work for later commits, e.g. `609022747856c619984fa972e6a773259625a9ec` as used for UPRN agent and provided `.war` file)

The Thematic Surface Discovery Agent (TSDA) is used to enrich LOD1 building data with thematic surface information (i.e. roof, wall, floor, etc.) and also provides a `LOD0 Footprint` mode to attach an `lod0FootprintId` to each building (required for subsequent UPRN agent). The [TSDAgent] README provides details about building and using the TSDA. Please note that there are discrepancies between the Access Agent related parts in the [TSDAgent] README and the [AccessAgent] README, with the [AccessAgent] README being up-to-date.

An example request to the TSD Agent in footprint mode is provided below:
```
PUT http://localhost:8080/agents/discovery/thematicsurface
Content-Type: application/json

{ "namespace": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/",
  "mode": "footprint" }
```

- A KG export after successfully amended by the TSD Agent is provided in `DATA/99 KG snapshots/2_ontocitygml_tsd`

## 1.6) UPRN Agent (potentially in chunks)

> The following steps refer to commit `609022747856c619984fa972e6a773259625a9ec` on `https://github.com/cambridge-cares/CitiesKG/tree/develop` (the `.war` file built of this commit is provided in the `DATA/03 OntoCityGml Instantiation/City agents war file/` repository (for reference); however, deploying via IntelliJ is recommended).

The [UPRN Agent] queries intersecting UPRNs for each instantiated OntoCityGml building from the Ordnance Survey [OS Features API] and instantiates them into the KG. The agent is designed to either process all buildings within a namespace (and push changes to KG every 2k buildings) or a single building provided as `cityObjectIRI`:

```
PUT http://localhost:8080/agents/uprn
Content-Type: application/json

{ "namespace":"http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/",
  // Optional key-value pair to specify a single building to be processed
  "cityObjectIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_0004923f-7eed-419e-aa0c-19a9974be52e/" }
```
As the agent tends to fail when processing an entire namespace on particular machines (heap space issues or "arbitrary "JSON exceptions have been observed), a workaround might be necessary. The Kings Lynn [Utilities] repository contains a script to run the [UPRN Agent in batches] of single buildings with a to be specified waiting time between individual requests (to allow for uninterrupted SPARQL updates). More details can be found in the README there.

- A KG export after successfully amended by the UPRN Agent is provided in `DATA/99 KG snapshots/3_ontocitygml_tsd_uprn`

&nbsp;
## 2) Building data enrichment (OntoBuiltEnv)

The following section provides a step-by-step guide on how to deploy a series of further agents to enrich the sole geospatial building information of OntoCityGML. Each agents is deployed as Docker container to the spun up stack, with a few environment variables to be defined in each `docker-compose.yml` file. An overview of the to-be used Blazegraph namespaces and PostGIS table names is provided in the table below. Furthermore, the [Agent docker-compose file folder] contains all actually used `docker-compose.yml` files.

Some of the agents have (data) interdependencies and, hence, require matching namespaces and/or PostGIS table names, i.e.
- All EPC, Land Registry and Flood Warning data needs to go into the same namespace for the Derivation Agents to pick up respective updates
- Flood Warning Instantiation data needs access to previously instantiated buildings in PostGIS to mark-up affected buildings
- Property Sales Instantiation and Property Value Estimation agents need access to previously instantiated buildings in PostGIS to create virtual Geoserver tables with property values

&nbsp;
| Agent | Blazegraph namespace | PostGIS table(s) | Geoserver layer | Geoserver workspace |
|---|---|---|---|---|
| EPC Instantiation Agent | kingslynn | buildings | buildings | kingslynn |
| Property Sales Instantiation | kingslynn | transactions<br>(buildings) | transactions | kingslynn |
| Flood Warnings Instantiation Agent | kingslynn | floodwarnings<br>(buildings) | floodwarnings | kingslynn |
| Average Square Metre Price Agent | kingslynn |  |  |  |
| Property Value Estimation Agent | kingslynn | marketvalues<br>(buildings) | marketvalues | kingslynn |
| Flood Assessment Agent | kingslynn | floodwarnings<br>population |  |  |
| MetOffice Agent | metoffice | metoffice | metoffice | stations |
| Riverlevel Agent | river_stations | river_stations | river_stations | stations |
| Airquality Agent | airquality | airquality | airquality | stations |

&nbsp;
## 2.1) Energy Performance Certificate (EPC) Agent

Deploy the agent as described in the [EPC Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_epcs.yml` in the [Agent docker-compose file folder] for the actually used compose file.

The required Base64-encoded authentication token can be created using `echo -n '<email address>:<provided API key>' | base64` on the command line (Windows) with `<email address>` being the email address used to register for the API and `<provided API key>` being the API key sent to you via email after registration. In case `bash ./stack.sh start KINGS-LYNN` does not successfully start the agent, simply re-run the command (this is a known issue and mentioned in the [EPC Agent] README). 

After agent startup, follow the described instantiation workflow by sending the respective HTTP requests to the agent. The subsequent recurring update of instantiated data occurs automatically. The EPC Agent represents the first step/agent to instantiate non-OntoCityGml buildings data in the overall workflow.

0) New namespace (i.e. `kingslynn`) to host all building related data will be created automatically upon agent startup (incl. upload of ontology and all required unit symbols)
1) Instantiate all postcodes in King's Lynn local authority:
    ```
    POST http://165.232.172.16:5007/epcagent/instantiate/postcodes
    Content-Type: application/json

    { "query": {
        "district": "E07000146"
        }
    }
    ```
2) Instantiate all EPC building data (for all buildings and from all 3 APIs):
    ```
    POST http://165.232.172.16:5007/epcagent/instantiate/certificates/all
    Content-Type: application/json

    { "query": {
        "ocgml_endpoint": "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql"
        }
    }
    ```
3) Run Building Matching Agent (details see below)
4) Update geospatial representation of buildings and insert additional OntoCityGml information in OntoBuiltEnv namespace (i.e. `kingslynn`), required for DTVF and Geoserver styling:
    ```
    POST http://165.232.172.16:5007/epcagent/add/ocgml_info
    Content-Type: application/json
    ```

- There are KG exports of successfully instantiated EPC data provided in 
    - steps 1 & 2: `DATA/99 KG snapshots/4_epc_data_before_matching`
    - steps 1, 2 & 3: `DATA/99 KG snapshots/5_epc_data_after_matching`
- There is no KG export after step 4, as this also requires the footprints to be uploaded to PostGIS (for visualisation); hence, step 4 shall be run as part of the instantiation workflow


## 2.2) Building Matching Agent

> The following description refers to commit `8cb656055ea74410ef3c4c0764a6c0a80efc38ff` on `https://github.com/cambridge-cares/TheWorldAvatar/tree/main`

The Building Matching Agent links buildings instantiated according to OntoBuiltEnv using the EPC Agent with their OntoCityGml representations. General details on how to use the agent can be found in the [Building Matching Readme]; however, all relevant steps are also described in section 3.4. in the [EPC Agent] README. The following request shall match buildings in the `ocgml` and `kingslynn` namespace:

```
PUT http://localhost:58085/BuildingMatchingAgent/match
Content-Type: application/json

{ 
  "ocgml": "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql",
  "obe": "http://165.232.172.16:3838/blazegraph/namespace/kingslynn/sparql",
  "prefixIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
}
```

After the Building instances are matched, step 3.4) from the EPC Agent can be performed.

- A KG export of successfully instantiated and linked EPC data is provided in `DATA/99 KG snapshots/5_epc_data_after_matching` (i.e. this does NOT include step 3.5 from the EPC agent README, but only the matching of buildings)

## 2.3) Property Sales Instantiation Agent

To avoid potential issues with unregistered and unavailable derivation agents, both the `Average Square Metre Price Agent` and the `Property Value Estimation Agent` should be deployed (and hence registered in the KG) **before** instantiating property transaction data. Otherwise, the `createSyncDerivationForNewInfo` method will cause an exception, as it cannot instantiate the requested derivation outputs.

Deploy the agent as described in the [Property Sales Instantiation Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_landregistry.yml` in the [Agent docker-compose file folder] for the actually used compose file.

After agent startup, the following request shall instantiate the latest property sales transactions for all instantiated buildings (if available, matched by address). The agent also instantiates the derivation markup for the Average Square Metre Price per Postcode as well as the Property Value Estimate per Building and triggers the first assessment of both quantities. The request requires the EPC Agent to be run first in the same namespace to instantiate the buildings.

```
POST http://165.232.172.16:5008/landregistry/update_all
Content-Type: application/json

{ "query": {
      "min_confidence": 95
    }
}
```

&nbsp;
## 3) Flood assessment (derivation agents)

## 3.1) Average Square Metre Price Agent

Deploy the agent as described in the [Average Square Metre Price Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_avg_sqm_price.yml` in the [Agent docker-compose file folder] for the actually used compose file.

After agent startup, the agent starts monitoring the specified namespace for outdated information in the specified frequency (i.e. asynchronous mode) or triggers a re-assessment upon request (i.e. synchronous mode).

## 3.2) Property Value Estimation Agent

Deploy the agent as described in the [Property Sales Instantiation Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_value_estimation.yml` in the [Agent docker-compose file folder] for the actually used compose file.

After agent startup, the agent starts monitoring the specified namespace for outdated information in the specified frequency (i.e. asynchronous mode) or triggers a re-assessment upon request (i.e. synchronous mode).

## 3.3) Flood Assessment Agent

Deploy the agent as described in the [Flood Assessment Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_flood_assessment.yml` in the [Agent docker-compose file folder] for the actually used compose file.

After agent startup, the agent starts monitoring the specified namespace for changes in instantiated OntoFlood and OntoBuiltEnv properties and automatically updates the associated potential Impacts of a flood (i.e. using the asychronous mode of the Derivation Framework).

## 3.4) Flood Warning Instantiation Agent

Deploy the agent as described in the [Flood Warning Instantiation Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_flood_warnings.yml` in the [Agent docker-compose file folder] for the actually used compose file.

Agent start-up will automatically register a recurring task to assimilate latest flood alerts and warning on an hourly basis in the background. Newly instantiated or updated flood alerts/warning shall trigger a new derivation cascade to update the associated impacts of a flood.

&nbsp;
## 4) Additional data incorporation

## 4.1) MetOffice Agent

The [MetOffice Agent] continuously (i.e. once per day) queries data from the MetOffice API and instantiates it according to the OntoEMS ontology. Deploy the agent as described in the [MetOffice Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_metoffice.yml` in the [Agent docker-compose file folder] for the actually used compose file. The required API key can be obtained from the [MetOffice My Account] page after successful registration.

After agent startup, latest weather data will be instantiated/updated once the cronjob is triggered (i.e. currently at 03:00am UTC). An initial data instantiation can be invoked by sending the following HTTP request to the agent:
```
GET http://165.232.172.16:5001/api/metofficeagent/update/all
Content-Type: application/json
```

## 4.2) River Levels Agent

The [RiverLevelsAgent] (also referred to as *Flood Agent*) instantiates river level data from the [Environment Agency] into the KG. Details on building and deploying the agent are provided in its README and only summarised here: 

* Create Blazegraph namespace specified in the docker-compose file (i.e. `river_stations`) before spinning up the agent
* Both the `datum.json` and `river_stations.csv` files provided in the [river_level_agent input folder] shall be copied over to the root directory of the agent (i.e. the location where the agent's `docker-compose.yml` file is located)
* To deploy the agent (using the pulled image) to the spun up stack, simply run the following command to initialise the stations and start a scheduled update that downloads data from the API daily. See the `docker-compose_riverstations.yml` in the [Agent docker-compose file folder] for the actually used compose file:
    ```bash
    bash ./stack.sh start KINGS-LYNN
    ```

## 4.3) AirQuality Agent

The [AirQuality Agent] continuously (i.e. once per day) queries data from the UK-AIR Sensor Observation Service API and instantiates it according to the OntoEMS ontology. Deploy the agent as described in the [AirQuality Agent] README, i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack by running `bash ./stack.sh start KINGS-LYNN` inside the agent repository. See the `docker-compose_airquality.yml` in the [Agent docker-compose file folder] for the actually used compose file.

After agent startup, latest airquality observations will be instantiated/updated once the cronjob is triggered (i.e. currently at 3:00am UTC). An initial data instantiation can be invoked by sending the following HTTP request to the agent:
```
GET http://165.232.172.16:5002/airqualityagent/update/all
Content-Type: application/json
```

As the UK-AIR Sensor Observation Service API (currently) does not cover King's Lynn, virtual (mocked) station(s) can be instantiated by sending the following request. Currently, this instantiates one mocked station which shows readings from the actual station in Aberdeen Erroll Park. This is an arbitrary choice and can be changed [here](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/AirQualityAgent/agent/datainstantiation/stations.py#L197)
```
GET http://165.232.172.16:5002/airqualityagent/instantiate/mocked
Content-Type: application/json
```

&nbsp;
# 4. Tracking instantiated building information

The [resources] folder contains an `instantiated_buildings.sparql` file which contains several SPARQL queries to track the instantiation process. It primarily helps to identify how many buildings are instantiated at all, how many buildings possess EPC information, and how many buildings have previous sales transaction information.

&nbsp;
# 5. Triggering new derivation cascades

There are two ways to trigger new derivation cascades (e.g. for visualisation purposes) on demand. **Please note** that the respective updates in the property value and flood impact estimates only become available after the asynchronous `Flood Assessment Agent` has processed them (update frequency set in respective docker-compose file).

## 1) Instantiate/update mocked flood alerts/warnings in the vicinity of King's Lynn

There are a few mocked API responses for the Environment Agency flood-monitoring API which instantiate/update flood warnings for different areas and varying severities. The alerts/warnings can be instantiated using the HTTP request to the agent below, with more details to be found in the [Flood Warning Agent resources folder].

```
POST http://165.232.172.16:5009/floodwarnings/update/all
Content-Type: application/json

{ "query": {
      "file_path": "/app/mock_api_responses/west_warning1.json"
    }
}
```

## 2) Update the instantiated Property Price Index by scaling it with a predefined value

The `Property Sales Instantiation Agent` provides an HTTP endpoint to manipulate the instantiated Property Price Index (PPI), which will in turn trigger an updated assessment of propert value estimates. The below request can be used to scale the instantiated data, with more details to be found in the [Property Sales Instantiation Agent resources folder].

```
POST http://165.232.172.16:5008/landregistry/scale_ppi
Content-Type: application/json

{ "query": {
      "ppi_iri": "<placeholder>",
      "months": 1,
      "scaler": 2.0,
      "request_update": true
    }
}
```


&nbsp;
# 6. Redeployment

It has been observed that the stack tends to crash when RAM usage approaches 100%. Depending on when the stack has been created initially and how the `stack-manager` development has progressed since then, simple restarting the stack from `main` might not be sufficient due to potential version conflicts (e.g., it has been observed that initially created data volumes using PostGIS 14 are not compatible with PostGIS 15, which is used as of `stack-manager` version >1.13.3). To restart the stack using a specific version of the `stack-manager`, simply specify the `REQUIRED_VERSION` in the provided `redeploy.sh` convenience script and run it from the repository where this README is located.

Please note that this 1) only works after the initial data instantiation workflow has been executed (due to initial agent dependencies as explained above) and 2) requires all corresponding `docker-compose` files populated with all required parameters (i.e., aligned namespaces and API keys) in the corresponding agent repositories.

&nbsp;
# 7. Connecting with CreDo visualisation

The visualisation includes synthetic network data created during CReDo and hosted by CMCL. In case this data does not show up in the visualisation, this is likely because new scenarios have been created and the visualisation needs to be updated accordingly. The list of latest scenarios can be inspected [here](https://kg.cmclinnovations.com/credo/phase2/central/CentralStackAgent/getScenarios?type=list).

To update the visualisation, the scenario id part of the `data` endpoints (i.e. `gxibUm4A` in the example below) in the [data.json] need to be updated with the corresponding id from the above link.

```
"name": "Water Network (Synthetic)",
"stack": "https://kg.cmclinnovations.com/credo/phase2/water",
"sources": [
    {
        "id": "water-source",
        "type": "geojson",
        "data": "https://kg.cmclinnovations.com/credo/phase2/water/geoserver/gxibUm4A/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gxibUm4A%3AWater&outputFormat=application%2Fjson",
        "cluster": true,
```

&nbsp;
# Potential refinements/next steps

**EPC Agent**
- HTTP response to `/epcagent/instantiate/certificates/all` does not seem to provide the correct number of instantiated and updated properties. Instead of providing the sum of all instantiated/updated properties from all API endpoints, it only seems to provide the number from the last endpoint and shall be revisited
- There seems to be a very minor fraction of properties which are classified as Flat and Building/Property at the same time.
- There is a very minor fraction of properties (less than 10 in total of ~13300), which do have multiple address details instantiated, e.g. 2 associated street names. This can cause issues with the HM Land Registry Agent when instantiating sales transactions, as the applicable property is determined by address matching and the instantiation of multiple possible addresses adds ambiguity here. This is currently handled by dropping duplicated addresses for the same property inside the Landregistry Agent, but should ideally be fixed on the instantaition side. The issue seems to affect mainly parent buildings with only one child property/flat.
To identify potentially affected properties, the following SPARQL queries can be used:
```
prefix obe:<https://www.theworldavatar.com/kg/ontobuiltenv/>
prefix icontact:<http://ontology.eil.utoronto.ca/icontact.owl#>

SELECT ?property (count(?tx) as ?count)
WHERE {
  ?property rdf:type/rdfs:subClassOf* obe:Property ;
            obe:hasLatestTransactionRecord ?tx .
}
GROUP BY ?property
HAVING(?count > 1)
```
```
prefix obe:<https://www.theworldavatar.com/kg/ontobuiltenv/>
prefix icontact:<http://ontology.eil.utoronto.ca/icontact.owl#>

SELECT distinct ?address (count(distinct ?prop) as ?properties) (count(distinct ?street) as ?streets)
WHERE {
  ?prop obe:hasAddress ?address .
  ?address a icontact:Address ;
           icontact:hasStreet ?street
}
GROUP BY ?address
HAVING(?streets > 1)
```

**Property Value Estimation Agent**
- There can be occasions where there are "too recent" actual property sales transactions with a date beyond the scope of the Property Price Index (PPI) are instantiated (as the PPI is always releassed with some lead time of ~2 months). In such a case the Property Value Estimation Agent will raise a KeyError (e.g. KeyError: '2023-02') and instantiate the property value as non-computable. As this only affects a very minor fraction of properties, it is not considered a major issue at the moment, but could be solved by simply using the latest available PPI value instead of raising an error.


<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Environment Agency]: https://environment.data.gov.uk/flood-monitoring/doc/reference
[forwarding the port]: https://code.visualstudio.com/docs/remote/ssh#_forwarding-a-port-creating-ssh-tunnel
[OS Features API]: https://api.os.uk/features/
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[MetOffice My Account]: https://register.metoffice.gov.uk/MyAccountClient/account/view

<!-- Stack references -->
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Stack data uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md

<!-- Agents -->
[AccessAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent
[CityImportAgent]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[TSDAgent]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[UPRN Agent]: https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent
[Building Matching Readme]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BuildingMatchingAgent/README.md
[EPC Agent]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/EnergyPerformanceCertificateAgent/README.md
[Average Square Metre Price Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AverageSquareMetrePriceAgent/README.md
[Property Value Estimation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent/README.md
[Flood Assessment Agent]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/FloodAssessmentAgent/README.md
[Property Sales Instantiation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HMLandRegistryAgent/README.md
[Property Sales Instantiation Agent resources folder]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HMLandRegistryAgent/resources
[Flood Warning Instantiation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodWarningAgent/README.md
[Flood Warning Agent resources folder]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodWarningAgent/resources
[MetOffice Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MetOfficeAgent
[RiverLevelsAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodAgent
[AirQuality Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AirQualityAgent

<!-- Repositories -->
[data.json]: /StackDeployment/inputs/stack-manager/inputs/data/visualisation/data.json
[Agent docker-compose file folder]: /StackDeployment/inputs/docker_compose_files
[resources]: /StackDeployment/resources
[river_level_agent input folder]: /StackDeployment/inputs/river_level_agent
[UPRN Agent in batches]: ../Utilities/uprn_agent/run_uprn_agent_in_chunks.py
[Utilities]: ../Utilities

<!-- Files -->
[routing.json]: /StackDeployment/inputs/access_agent/routing.json
[CKG config.properties]: https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/src/main/resources/config.properties