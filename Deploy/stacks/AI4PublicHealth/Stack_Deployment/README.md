# AI for Public Health: Stack Deployment

This project contains a step-by-step guide on how to spin up the Docker stack for the AI for Public Health project and instantiate all relevant data. It links to other projects and helper scripts where appropriate.

Section Overview:
- [1. Prerequisites](#1-prerequisites): Preparations required before spinning up the use case stack
- [2. Spinning up the stack](#2-spinning-up-the-stack): How to spin up the core stack 
- [3. Data instantiation workflow](#3-data-instantiation-workflow): How to upload initial data sets and deploy all required agents.
- [4. Visualisation](#4-visualisation): How to visualise instantiated cross-domain data via TWA tools
- [Potential refinements/next steps](#potential-refinementsnext-steps): Potential refinements for future work

# 1. Prerequisites

## Required software installation

Ensure the following software is installed:

- [Git](https://git-scm.com/downloads)
- [Docker Desktop](https://docs.docker.com/get-docker/) or Docker Engine and Docker Compose Plugin

## Access to Docker registries

Access to the [GitHub Container Registry] is required to pull images for spinning up the Docker stack. Ensure this access is granted beforehand by configuring a GitHub [personal access token], which must have a `scope` that [allows you to publish and install packages].

To authenticate with the container registry, execute the following command and provide your personal access token when prompted to establish the connection.

```bash
# Github Container registry
$ docker login ghcr.io -u <github_username>
$ <github_personal_access_token>
```

# 2. Spinning up the stack

This section explains how to spin up the core stack. If using VSCode, ensure that all required VSCode extensions (e.g., [Remote - SSH], [Docker], and [REST Client]) are installed to enable all convenience scripts to function correctly.

## Spinning up the stack locally
Before spinning up the stack using the [Stack Manager], please provide the following files in `./stack-manager/inputs/secrets`:

- `postgis_password`
- `geoserver_password`

Additionally, because this stack includes the [Fenland Trajectory Agent] as a service to instantiate GPS trajectories, and this agent's image cannot be pulled directly from the container registry, it needs to be built locally before spinning up the project stack. Please copy the JSON file named `fenland-trajectory-agent.json` from `Agents/FenlandTrajectoryAgent/stack-manager-input-config-service/` and paste it into `Deploy/stacks/dynamic/stack-manager/inputs/config/services`. Then, navigate to the directory `Agents/FenlandTrajectoryAgent` and execute the following command in a bash terminal to build the agent:

```
bash ./stack.sh build
```
Afterwards, copy all files in `./stack-manager/inputs` into their corresponding repositories of the [Stack Manager] tool under `Deploy/stacks/dynamic/stack-manager/`. Then navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start Healthcare
# Stop the stack
bash ./stack.sh stop Healthcare
# Remove the stack (incl. volumes)
bash ./stack.sh remove Healthcare -v
# Remove individual service
bash ./stack.sh remove Healthcare <service name>
```
After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. Adminer, Ontop, Blazegraph, Geoserver). The exact endpoints and login details can be found in the [Stack Manager README].

## Spinning up the stack remotely via SSH

To deploy the stack remotely via SSH, use VSCode's built-in SSH support. Follow the steps in [VSCode via SSH] to connect to a remote machine (e.g., a virtual machine on Digital Ocean) and start deployment. Regular log in relies on username and password, You can also consider generating an [SSH key] and uploading it via [Upload SSH key] to enable automatic authentication and eliminate repeated credential prompts. After logging in, [git clone] a remote copy of [The World Avatar] repository and follow the provided instructions to deploy the stack.

To access deployed containers through exposed endpoints (e.g., `http://<host IP>:3840/ontop/ui`), ensure the necessary ports are open on the remote machine. Please request port changes through your server administrator—avoid directly modifying firewall rules on individual droplets using tools like `ufw` or `iptables`.

When interacting with the GeoServer GUI remotely, some issues may arise (e.g., inability to remove layers or edit CRS information). To address this, consider [forwarding the port] used by the stack to your local machine after establishing an [SSH tunnel]. This will make GeoServer accessible at `http://localhost:{port}/geoserver/` instead of a remote address like `http://<host IP>:{port}/geoserver/`. 

# 3. Data instantiation workflow

The following provides an overview of all steps and agents required to instantiate the environmental features and phyical activity data. 

## 3.1 Food Hygiene Ratings (.xml)
The UK Food Hygiene Rating Scheme (FHRS) data includes hygiene ratings or inspection results for businesses such as restaurants, cafes, and supermarkets. This data reflects food hygiene standards observed during inspections conducted by local authorities and is available in XML format via a public API. 

### 1) XML converter
To integrate this environmental feature, the [xml_converter] processes the its raw online XML file, extracts relevant fields, and converts them into a structured CSV format. This ensures compatibility with subsequent data upload and querying steps.

### 2) Stack data uploader
Once converted, the Food Hygiene Ratings data in CSV format is uploaded to the stack using the [Stack Data Uploader]. Firstly, navigate to the `Deploy/stacks/AI4PublicHealth/Stack_Deployment/stack-data-uploader` directory. Copy the configuration files from the `config` directory to the corresponding directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/config/`. Then replace the `readme.txt` files in the `Deploy/stacks/AI4PublicHealth/Stack_Deployment/stack-data-uploader/data/FoodHygiene` sub-folders with the CSV file obtained by the [xml_converter]. In addition, copy the [FoodHygieneRating] file from [mapping folder] and paste to `Deploy/stacks/dynamic/stack-data-uploader`.

After completing these steps, navigate to `Deploy/stacks/dynamic/stack-data-uploader`. From this directory, execute the following command in a *bash* terminal and wait until the container stops, indicating that the upload has finished. 

```bash
bash ./stack.sh start Healthcare
```

### 3) SPARQL query via virtual knowledge graph
The stack provides an Ontop SPARQL endpoint for querying Food Hygiene Ratings semantically. Leveraging the Ontology of Food Hygiene Rating Scheme ([OntoFHRS]) and a defined OBDA mapping titled [FoodHygieneRating], the endpoint maps relational database records stored in PostGIS, such as business details, geolocation, and hygiene ratings, into a semantic framework. This allows queries to retrieve structured information like business names, inspection ratings, and geographic coordinates, supporting analyses across geographic scales and enabling integration with other datasets.

## 3.2) Greenspace (.shp) and Points of Interest (.csv)

The data used in this project is sourced from [OS Open Greenspace] and [Points of Interest (POI)] datasets. The [OS Open Greenspace] dataset provides a comprehensive mapping of greenspaces across Great Britain, including 
public parks, sports facilities, and allotments, along with access points. Similarly, the [Points of Interest 
(POI)] dataset includes detailed information about commercial and non-commercial establishments, such as 
businesses, retail spaces, and service providers, with attributes like phone numbers, web URLs, and 
classifications. 

### 1) Greenspace data upload

The greenspace data used in this project is available via [Dropbox][greenspace-dropbox]. Upon downloading and extracting the archive file, navigate to the `open-greenspace_5585681` directory. Subsequently, copy the greenspace vector data files (namely, `GB_GreenspaceSite.dbf`, `GB_GreenspaceSite.prj`, `GB_GreenspaceSite.shp`, and `GB_GreenspaceSite.shx`) to the target data directory `stack-data-uploader/inputs/data/GB_GreenspaceSite/vector/`. Should the target directory not exist, please create it prior to copying the files.

> :warning: **Note:** Some [OS Open Greenspace] data may not have the `MULTIPOLYGON` geometry type set. If you encounter errors about `MULTIPOLYGON` and `point` type mismatches in the logs, remove the `"-nlt": ["MULTIPOLYGON"]` line from the configuration file `stack-data-uploader/inputs/config/GBGreenspaceSite.json`.

Copy configuration files from `config` to `Deploy/stacks/dynamic/stack-data-uploader/inputs/config/`. If using OBDA mappings, copy the `.obda` files from the [mapping folder] to `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/GB_GreenspaceSite/`.

Navigate to `Deploy/stacks/dynamic/stack-data-uploader` and execute:

```bash
bash ./stack.sh start Healthcare
```

### 2) Points of Interest data upload

Similar to the Food Hygiene Rating data, the instantiation of the Points of Interest dataset also relies on the [Stack Data Uploader]. Replace the `readme.txt` files in the `data` sub-folders with the POI CSV files.

### 3) SPARQL query via virtual knowledge graph

The Ontop endpoint is also employed to support querying Greenspace and Points of Interest data semantically. Using [OntoGreenspace] and [OntoPOI] along with defined OBDA mappings, the endpoint semantically represents relational database records, including greenspace locations, access points, business details, and geographic coordinates.

## 3.3) GPS Trajectories (.csv)
Individual GPS trajectories used in this study are instantiated using the [Fenland Trajectory Agent]. This agent extracts key attributes such as latitude, longitude, speed, heading, and time from CSV files, organises them into triples using the Ontology of Devices ([OntoDevice]), and uploads the data into the knowledge graph and a relational database. Details on deploying this agent can be found [here].

# 4. Visualisation
The visualisation for this project is powered by the TWA Visualisation Platform ([TWA-VP]). To deploy the TWA-VP stack, first copy the folder named `config` from the `public` folder located at `Deploy/stacks/AI4PublicHealth/TWA-VP/public/` and paste it into the directory `web/twa-vis-platform/code/public/`. Next, copy the folder named `Fenland` from `Deploy/stacks/AI4PublicHealth/TWA-VP/public/images/` and paste it into `web/twa-vis-platform/code/public/images/`. Then, navigate to the `web/twa-vis-platform` directory and create the following files:

- `mapbox_username`
- `mapbox_api_key`

Populate these files with your Mapbox username and API key. After that, execute the following command in the terminal:

```bash
docker compose up -d
```
The default port for the TWA-VP is 3000. Please ensure this port is open before deploying the visualisation stack. It is worth mentioning that, as of the time of writing, the visualisation stack is configured with hard-coded IP addresses for its data sources. Even after completing the data instantiation and uploading steps outlined in Section 3, the visualisation will, by default, display data from these hard-coded IP addresses (if they are active), rather than the data you have instantiated. To display your own data, you need to update the visualisation configuration files to point to your data sources. The file used to specify data layers is `data-twa-vp-demo.json`, located at `\AI4PublicHealth\TWA-VP\public\config\data-twa-vp-demo.json`. Refer to the [Mapbox visualisation guidance] for the standard way to add a new layer or change icons.

# Potential refinements/next steps
To further advance this project, the next steps involve developing a high-level exposure ontology to semantically represent environmental data and exposure quantifications, enabling the integration of additional datasets and the automated assessment of exposure. In parallel, SPARQL queries will be developed to quantify environmental exposure metrics, supporting spatial and temporal analyses that establish links between environmental factors and public health outcomes.

# TODO

- Ensure consistency of `datasetDirectory` of all the point of interest stack data uploader `json` config files. Some of them currently point at the wrong place.
- Merge this stack into the `hd4-stack`.

## Authors ##
Jiying Chen (jc2341@cam.ac.uk), Nov 2024

<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[GitHub Container Registry]: https://github.com/orgs/cambridge-cares/packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Environment Agency]: https://environment.data.gov.uk/flood-monitoring/doc/reference
[forwarding the port]: https://code.visualstudio.com/docs/remote/ssh#_forwarding-a-port-creating-ssh-tunnel
[OS Features API]: https://api.os.uk/features/
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[MetOffice My Account]: https://register.metoffice.gov.uk/MyAccountClient/account/view
[Remote - SSH]: https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh
[Docker]: https://code.visualstudio.com/docs/containers/overview
[REST Client]: https://marketplace.visualstudio.com/items?itemName=humao.rest-client
[Stack Manager README]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[OntoFHRS]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-ai4ph-ontologies/JPS_Ontology/ontology/ontofhrs
[OntoPOI]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-ai4ph-ontologies/JPS_Ontology/ontology/ontopoi
[OntoGreenspace]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-ai4ph-ontologies/JPS_Ontology/ontology/ontogreenspace
[fia-queries]:./stack-manager/inputs/data/fia-queries/
[webspace-mapbox]: ./stack-manager/inputs/data/webspace-mapbox/
[TWA-VF]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework
[TWA-VP]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform
[OS Open Greenspace]: https://www.ordnancesurvey.co.uk/products/os-open-greenspace
[Points of Interest (POI)]: https://www.ordnancesurvey.co.uk/products/points-of-interest
[Mapbox visualisation guidance]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/web/twa-vis-framework/docs/mapbox.md

<!-- Stack references -->
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Stack Data Uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md

<!-- Agents -->
[AccessAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent
[Fenland Trajectory Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FenlandTrajectoryAgent
[MetOffice Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MetOfficeAgent
[AirQuality Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AirQualityAgent

<!-- Files -->
[xml_converter]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-AI-for-Healthcare/Deploy/stacks/AI4PublicHealth/Common_Script/xml_converter
[FoodHygieneRating]: ./stack-data-uploader/obda_mappings/FoodHygieneRating.obda
[here]: https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-AI-for-Healthcare/Agents/FenlandTrajectoryAgent/README.md
[OntoDevice]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-ai4ph-ontologies/JPS_Ontology/ontology/ontodevice
[mapping folder]: ./stack-data-uploader/obda_mappings
[greenspace-dropbox]: https://www.dropbox.com/scl/fi/sv7lqoc9xq3dnk6u0tlng/Download_AI_project_PA_OS_Open_Greenspace_2550192.zip?rlkey=ciw5cs9ju4lko6usj3iq4f9qi&st=c3qovqbp&dl=0
