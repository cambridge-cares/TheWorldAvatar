# Kings Lynn Stack

This project contains a step-by-step guide on how to spin up the Docker Stack (developed by CMCL) for the King's Lynn use case and instantiate all relevant data. It links to other projects and helper scripts where appropriate.

&nbsp;
## Prerequisites

### <u>Access to Docker registries</u>

Spinning up the (core) Docker Stack requires access to the [CMCL Docker Registry] to pull required images. Deploying (pre-built) agents to the spun up Stack requires access to CARES' [Container registry on Github] to pull agent images. Access needs to be ensured beforehand via your Github [personal access token], which must have a `scope` that [allows you to publish and install packages].

To log in to the Container registries, please run the following commands to establish the connections and provide your password/access token when prompted. For more details please refer to the linked resources.
```bash
  # CMCL Container registry
  $ docker login docker.cmclinnovations.com -u <username>
  $ <password>

  # Github Container registry
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

&nbsp;
# Spinning up the Stack

This section explains how to spin up the core stack and upload initial data sets, i.e. high-resolution population raster data and pre-instantiated OntoCityGml building triples.
If using VSCode, all required VSCode extensions shall be installed (on the remote machine if applicable) for all convenience scripts to work properly, i.e. *augustocdias.tasks-shell-input*.

> The functionality has been tested using the Stack Manager `docker.cmclinnovations.com/stack-manager:1.7.2` based on commit `2825c4c69c6543d88c687917c50bf965d3221da6` on branch `main` of the World Avatar repository.


&nbsp;
## Spinning up the core Stack

Navigate to `Deploy/stacks/dynamic/stack-manager` on the `main` branch of the World Avatar repository and run the following command there from a *bash* terminal. To spin up the stack using the [Stack manager], both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance following the provided link). There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start KINGS-LYNN

# Stop the stack
bash ./stack.sh stop KINGS-LYNN

# Remove stack services (incl. volumes)
bash ./stack.sh remove KINGS-LYNN -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [Stack manager] readme.

After stack startup, please ensure that Geoserver supports serving `MapBox Vector Tiles` as required by the Digital Twin Visualisation Framework. In case this option does not yet appear in the Geoserver GUI, please restart the Geoserver service (i.e. right click on container in VSCode Docker extension and stop container followed by another `bash ./stack.sh start KINGS-LYNN` from within the `stack-manager` repository). (It seems that the plug-in download URL required to enable `MapBox Vector Tiles` works but might have intermittent issues.)

&nbsp;
## Spinning up the core Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start deployment. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout <BRANCH NAME>
$ git pull
```
Once the repository clone is obtained, please follow the instructions above to spin up the stack on the remote machine. In order to access the exposed endpoints, e.g. `http://<host IP>:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

```bash
# Check permissions
ls -l <REPO NAME>
# Grant permissions
chmod -R +rwx <REPO NAME>
```

&nbsp;
## Uploading initial data

> The functionality has been tested using the Stack Data Uploader `docker.cmclinnovations.com/stack-data-uploader:1.7.2` based on commit `2825c4c69c6543d88c687917c50bf965d3221da6` on branch `main` of the World Avatar repository.

A few datasets and files which shall initially be uploaded to the stack are provided in the `inputs` folder of this repository. Uploading pre-instantiated OntoCityGml quads is optional but highly recommended to skip steps 1 - 4.2 (depending on the exact quads file provided) of the building instantiation workflow below.

The following steps explain how to upload the data to the stack using the [Stack data uploader] (please see the referenced README for more details):

1) Copy all relevant files from the `inputs/datauploader` folder of this repository into the `inputs` folder of  the stack data uploader repository:

    a) Copy the configuration files from the `inputs/datauploader/configs/` directory to the matching directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/configs/`

    b) Replace the `readme.txt` files in the `inputs/datauploader/data/*` sub-folders with the referenced data files from `../../Data/...`

    c) Copy all data sub-directories from the `inputs/datauploader/data` directory into the matching parent directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/`

2) Navigate to `Deploy/stacks/dynamic/stack-data-uploader` and run the following command there from a *bash* terminal and wait until container has stopped again (i.e. the upload has finished). Specified Blazegraph/Geoserver/... namespaces will be created automatically by the uploader if not already exist.
    ```bash
    bash ./stack.sh start KINGS-LYNN
    ```


&nbsp;
# Building instantiation workflow

## 1) Geospatial data consolidation (QGIS, *manual*)

QGIS is used to consolidate various geospatial data sets from Digimap (both Ordnance Survey (OS) Open data as well as Premium OS data are used, i.e. Building Height Attribute and Digital Terrain Model 5m ) into a single shapefile containing all relevant building information. The exact workflow is described in the `QGIS workflow.pptx` in the `../../Data/01 QGIS` repository, which also contains the QGIS project file. The output shapefile forms the input for the FME workflow below and can be found under `../../Data/02 FME/KingsLynn_cleaned ALL buildings_adjusted building heights_incl UPRNs_final.shp`.

## 2) Creation of .gml input file for KG import (FME, *manual*)

FME is used to convert the shapefile from the previous step into a `.gml` file that can be instantiated into the KG using the Import Agent/Importer Tool. The exact FME workflow is provided by `shapefile2citygml Kings Lynn BHA data_final.fmw` in the `../../Data/02 FME` repository, which also contains both its input and output file. The output `.gml` file contains (all) buildings in King's Lynn in LOD1 including their height (i.e. both building ground elevation as well as actual building height (is both premium data)) and UPRN information.

## 3) Importing building data into KG (CitiesKG Importer, *partially manual*)

> The following steps refer to commit `7c378e97d268b02e0d70661257894d5bff8e3655` on `https://github.com/cambridge-cares/CitiesKG/tree/develop`

The [CityImportAgent] can be used to import the `.gml` file from the previous step into the KG. However, the version at time of writing faces issues with larger `.gml` files and a manual workaround is required as detailed below. Please note that Java 8 and IntelliJ are required to build and run the CityImportAgent. Furthermore, the [AccessAgent] needs to be running locally (as Docker container) in order to access the target KG namespace. The folder `../../Data/03 OntoCityGml Instantiation/Standalone_CitiesKG_Blazegraph/` contains a `Start_Blazegraph_with_default_settings.bat` file to bring up a Blazegraph instance with required settings for importing OntoCityGml buildings, which will start at `http://127.0.0.1:9999/blazegraph/`.

It is **not** recommended to re-do step 3 and instead use the pre-instantiated OntoCityGml quads provided in the `../../Data/99 KG snapshots/1_instantiated_ontocitygml/` repository.

### 0) Access Agent

All CitiesKG Agents require the [AccessAgent] to be running (locally) as Docker container in order to access the target KG namespaces. Details on how to deploy the AccessAgent and upload required routing information are detailed in the [AccessAgent] README and summarised below:

1) Navigate to `JPS_ACCESS_AGENT/access-agent-dev-stack` on TWAs `main` branch
2) Pull Docker image and start container by running `docker-compose up -d --no-build`. The agent shall start at port `48888`
3) Replace initial `routing.json` within AccessAgent repository with provided [routing.json] file
4) Upload routing information by running `bash ./uploadRouting.sh` (again within `JPS_ACCESS_AGENT/access-agent-dev-stack` repo)

**Please note** that the `uri.route` within the [CKG config.properties] file needs to match the label used in the [routing.json] file, i.e. `uri.route=http://localhost:48888/ocgml_buildings`. After providing the correct `uri.route`, the City Agents `.war` file can be built and deployed as described in the [CityImportAgent] README.


### 1) City Import Agent

Build and deploy the City Import Agent as described in the [CityImportAgent] README. Required IntelliJ run configurations are provided in the `../../Data/03 OntoCityGml Instantiation/IntelliJ RunConfigurations/` repository, which also provides a short step-by-step guide `Building Instantiation_short.pptx`.

The used version at commit `7c378e97d268b02e0d70661257894d5bff8e3655` seems unable to handle large `.gml` files. Hence, the CityImportAgent is primarily used to split the large `.gml` file into multiple smaller `.gml` files to be manually uploaded by the Import GUI as described in the next step.

### 2) Import GUI

After the `.gml` file is split into smaller files, they can be manually uploaded in chunks of 100-200 files via the Importer GUI. The `Building Instantiation_short.pptx` guide contains a step-by-step description on how to achieve this with required IntelliJ run configurations also provided in the `../../Data/03 OntoCityGml Instantiation/IntelliJ RunConfigurations/` repository.

## <u>4) Building data enrichment</u>

## 4.1) Thematic Surface Discovery Agent (CitiesKG)

> The following steps refer to commit `7c378e97d268b02e0d70661257894d5bff8e3655` on `https://github.com/cambridge-cares/CitiesKG/tree/develop` (however, they should also work for later commits, e.g. `609022747856c619984fa972e6a773259625a9ec` as used for UPRN agent and provided `.war` file)

The Thematic Surface Discovery Agent (TSDA) is used to enrich LOD1 building data with thematic surface information (i.e. roof, wall, floor, etc.) and also provides a `LOD0 Footprint` mode to attach an `lod0FootprintId` to each building (required for subsequent UPRN agent). The [TSDAgent] README provides details about building and using the TSDA. Please note that there are discrepancies between the Access Agent related parts in the [TSDAgent] README and the [AccessAgent] README, with the [AccessAgent] README being up-to-date.

An example request to the TSD Agent in footprint mode is provided below:
```
PUT http://localhost:8080/agents/discovery/thematicsurface
Content-Type: application/json

{ "namespace": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/",
  "mode": "footprint" }
```

- A KG export after successfully amended by the TSD Agent is provided in `../../Data/99 KG snapshots/2_ontocitygml_tsd`

## 4.2) UPRN Agent (potentially in chunks)

> The following steps refer to commit `609022747856c619984fa972e6a773259625a9ec` on `https://github.com/cambridge-cares/CitiesKG/tree/develop` (the `.war` file built of this commit is provided in the `../../Data/03 OntoCityGml Instantiation/City agents war file/` repository (for reference); however, deploying via IntelliJ is recommended).

The [UPRN Agent] queries intersecting UPRNs for each instantiated OntoCityGml building from the Ordnance Survey [OS Features API] and instantiates them into the KG. The agent is designed to either process all buildings within a namespace (and push changes to KG every 2k buildings) or a single building provided as `cityObjectIRI`:

```
PUT http://localhost:8080/agents/uprn
Content-Type: application/json

{ "namespace":"http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/",
  // Optional key-value pair to specify a single building to be processed
  "cityObjectIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/building/UUID_0004923f-7eed-419e-aa0c-19a9974be52e/" }
```
As the agent tends to fail when processing an entire namespace on particular machines (heap space issues or "arbitrary "JSON exceptions have been observed), a workaround is necessary. The Kings Lynn [Utilities] repository contains a script to run the [UPRN Agent in batches] of single buildings with a to be specified waiting time between individual requests (to allow for uninterrupted SPARQL updates). More details can be found in the README there.

- A KG export after successfully amended by the UPRN Agent is provided in `../../Data/99 KG snapshots/3_ontocitygml_tsd_uprn`


## 4.3) Energy Performance Certificate (EPC) Agent

> The following steps refer to commit `???` on `https://github.com/cambridge-cares/TheWorldAvatar/tree/main` using the published Docker image `ghcr.io/cambridge-cares/epc_agent:1.0.0`

(Build and) deploy the EPC Agent as described in the [EPC Agent README], i.e. provide environment variables in the `docker-compose.yml` file and deploy the agent to the spun up stack. Follow the described instantiation workflow by sending the respective HTTP requests to the agent. The subsequent recurring updating of instantiated data occurs automatically.

0) New namespace (i.e. `buildings`) to host all building related data created automatically upon agent startup (incl. upload of ontology and all required unit symbols)
1) Instantiate all postcodes in King's Lynn local authority:
    ```
    POST http://165.232.172.16:5001/epcagent/instantiate/postcodes
    Content-Type: application/json

    { "query": {
        "district": "E07000146"
        }
    }
    ```
2) Instantiate all EPC building data (for all buildings and from all 3 APIs):
    ```
    POST http://165.232.172.16:5001/epcagent/instantiate/certificates/all
    Content-Type: application/json

    { "query": {
        "ocgml_endpoint": "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql"
        }
    }
    ```
3) Run Building Matching Agent (details see below)
4) Update geospatial representation of buildings and insert additional OntoCityGml information in OntoBuiltEnv namespace (required for DTVF and Geoserver styling)

- A KG export of successfully instantiated EPC data (steps 1 & 2) is provided in `../../Data/99 KG snapshots/4_epc_data_before_matching`


## 4.4) Building Matching Agent

> The following description refers to commit `7adc29459a2661f9fb7ad267d9f111d3d537249a` on `https://github.com/cambridge-cares/TheWorldAvatar/tree/main`

The Building Matching Agent links buildings instantiated according to OntoBuiltEnv using the EPC Agent with their OntoCityGml representations. General details on how to use the agent can be found in the [Building Matching Readme]; however, all relevant steps are also described in section 3.4. in the [EPC Agent README]. The following request shall match buildings in the `ocgml` and `buildings` namespace:

```
PUT http://localhost:58085/BuildingMatchingAgent/match
Content-Type: application/json

{ 
  "ocgml": "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql",
  "obe": "http://165.232.172.16:3838/blazegraph/namespace/buildings/sparql",
  "prefixIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
}
```

After the Building instances are matched, step 3) from the EPC Agent can be performed.

- A KG export of successfully instantiated and linked EPC data is provided in `../../Data/99 KG snapshots/5_epc_data_after_matching` (i.e. this also includes step 3 from the EPC agent)


## 4.5) Property Sales Instantiation Agent



## <u>5) Additional data incorporation </u>

## MetOffice Agent

> The following description refers to commit `6b3ff32af6df2c356e1a49f0c727ebf6db53a15a` on `https://github.com/cambridge-cares/TheWorldAvatar/tree/main` using the published Docker image `ghcr.io/cambridge-cares/metoffice_agent:1.0.0`

The [MetOffice Agent] continuously (i.e. once per day) queries data from the MetOffice API and instantiates it according to the OntoEMS ontology. To deploy the agent to the spun up `KINGS-LYNN` stack, please provide the target Blazegraph namespace, PostGIS/PostgreSQL database name, etc. in the [MetOffice docker-compose file]. Afterwards, simply run the following command from the [MetOffice Agent] repository (i.e. where the [MetOffice docker-compose file] is located) to deploy the agent using its published Docker image from the [Container registry on Github]:
```bash
bash ./stack.sh start KINGS-LYNN
```



## AirQuality Agent

## River Levels Agent

> The following description refers to commit `03bdd20501a9901d390c76fdd3b298f6ea672c66` on `https://github.com/cambridge-cares/TheWorldAvatar/tree/main`

The [RiverLevelsAgent] (also referred to as *Flood Agent*) instantiates river level data from the [Environment Agency] into the KG. Details on building and deploying the agent are provided in its README and only summarised here: 

* Building the agent requires both a `settings.xml` and `settings-security.xml` to be provided in the `docker/.m2` sub-repository of the agent to be able to download TWA packages from Github
* Both the `datum.json` and `river_stations.csv` files provided in the [RiverLevelAgent input folder] here shall be copied over to the root directory of the agent (i.e. the location where the agent's `docker-compose.yml` file is located)
* To deploy the agent to the spun up stack, simply run the following command to initialise the stations and start a scheduled update that downloads data from the API daily:
    ```bash
    bash ./stack.sh start KINGS-LYNN
    ```
```diff
- Please note: The agent populates all station data into the default "kb" namespace of Blazegraph
```

&nbsp;
# Tracking instantiated building information

The `resources` folder contains an `instantiated_buildings.sparql` file which contains several SPARQL queries to track the instantiation process. It primarily helps to identify how many buildings are instantiated at all, how many buildings possess EPC information, and how many buildings have previous sales transaction information.


<!-- Links -->
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Environment Agency]: https://environment.data.gov.uk/flood-monitoring/doc/reference
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[OS Features API]: https://api.os.uk/features/

<!-- Stack references -->
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Stack data uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader

<!-- Agents -->
[MetOffice Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MetOfficeAgent
[MetOffice docker-compose file]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/MetOfficeAgent/docker-compose.yml
[UPRN Agent]: https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent
[CityImportAgent]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[TSDAgent]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[AccessAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT#readme
[EPC Agent README]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/EnergyPerformanceCertificateAgent/README.md
[Building Matching Readme]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BuildingMatchingAgent/README.md
[RiverLevelsAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodAgent

<!-- Repositories -->
[Utilities]: ../Utilities
[UPRN Agent in batches]: ../Utilities/uprn_agent/run_uprn_agent_in_chunks.py
[RiverLevelAgent input folder]: /StackDeployment/inputs/RiverLevelAgent

<!-- Files -->
[routing.json]: /StackDeployment\inputs\AccessAgent\routing.json
[CKG config.properties]: https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/src/main/resources/config.properties