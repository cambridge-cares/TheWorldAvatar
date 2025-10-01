# 1. Description

The `Fenland Trajectory Agent` is a specialised tool designed to both instantiate structured time-series GPS trajectory data into a knowledge graph and perform quantitative exposure calculations between these trajectories and environmental features. This agent is implemented as a Docker container, designed for deployment within a stack managed by the [Stack Manager]. The agent receives [HTTP POST] requests or Client URL ([CURL]) commands to perform specified tasks. These include loading GPS trajectory files, mapping them into RDF triples, and uploading them to the stack. Additionally, for an exposure radius of interest, the agent can interpret environmental features and perform corresponding exposure calculations automatically. Examples of these calculations include determining the number of food retail locations or greenspace objects within the defined radius.

Presently, the agent focuses on data from the Fenland Study to analyze the interaction between GPS trajectories and environmental features within the context of a digital twin. Example gps data can be found at Dropbox/CoMo_shared/_Projects/c4e-AI-for-public-health/sample_gps_data.csv. By default, the data instantiated from the Fenland Study using this agent encompasses Speed, Height, Distance, Heading, Latitude, and Longitude. This method is also applicable to other categories of time-series structured data in Fenland Study by replacing or adding the relevant column names. The information instantiated into the knowledge graph adheres to the Ontology of Devices [OntoDevice] in [TheWorldAvatar] project. The instantiation process is executed based on the [TimeSeriesClient]. In terms of environmental features from the Fenland Study, the raw data is stored in Dropbox/CoMo_shared/_Projects/c4e-AI-for-public-health/Data/Raw data/. Uploading and instantiating environmental data requires the [Stack-data-uploader] tool, and configuration files can be found in the [AI-for-Public-Health] folder. 

# 2. Agent Setup

This section specifies the minimum requirements to build and deploy the Docker image of this agent.

## 2.1 Environment Variables
Before building and deploying the Docker image, several key properties need to be set in the [docker compose file] (further details and defaults are provided in the file):

```bash
#--- Deployment specific parameters ---#

STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver layer name, Also table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   # Name of the Workspace in Geoserver 
ONTOP_FILE            # Path to ontop mapping file (default: `/app/resources/ontop.obda`)

```

## 2.2 Access to GitHub's Container Registry

While building the Docker image of this agent, it also needs to pull images from the [Github container registry], or push images to GitHub if there are any local modifications to the agent made by the user. Access is required to be ensured beforehand via your github [personal access token], which must have a scope that [allows you to publish and install packages]. To log in to the [Github container registry] simply run the following command to establish the connection and provide the access token when prompted:

```
  $ docker login ghcr.io -u <github_username>
```
```
  $ <github_personal_access_token>
```

## 2.3 Stack Client Settings (Optional)
The Fenland Trajectory Agent requires importing [JPS_BASE_LIB] and [Stack-Clients] from the [twa] package. The [twa] package, formerly known as the [py4jps] package, serves as a [Python wrapper] capable of packaging compiled Java code into .jar files for use within Python. 
Currently, [twa] with version 0.0.1 exclusively packages [JPS_BASE_LIB]. To utilize [Stack-Clients] through [twa],the [Stack-Clients] resource needs to be added and allow for access. This adding procee can be executed automatically by [Dockerfile] employing jpsrm commands. 
However, occasionally commands executed within the Docker environment may not successfully package the components due to potential discrepancies between the Java and Python environments or specific configurations within Docker that interfere with the packaging process. If this occurs, the stack client can be manually configured following th instructions provided at the [JPS_Document]


# 3. Spinning Up the Agent

## 3.1 Preparing the GPS Data

The GPS trajectory data is structured and stored in tables (in .csv format). Each table consists of a series of record points, with each point containing eight essential pieces of information for instantiation, including UTC Date, UTC Time, Longitude (degrees), Latitude (degrees), Speed (km/h), Heading (degrees), Height (meters), and Distance (meters). Below is an example of the columns and values in GPS trajectory tables for instantiation:

| UTC DATE   | UTC TIME | LATITUDE  | LONGITUDE | SPEED  | HEADING | HEIGHT | DISTANCE |
|------------|----------|-----------|-----------|--------|---------|--------|----------|
| 2015/05/22 | 11:34:46 | 52.174374 | 0.137380  | 5.796  | 0       | 48.135 | 18.06    |
| 2015/05/22 | 11:34:56 | 52.174315 | 0.137153  | 5.785  | 0       | 48.611 | 16.86    |
| 2015/05/22 | 11:35:06 | 52.174194 | 0.137029  | 6.320  | 0       | 53.210 | 16.55    |
| ...        | ...      | ...       | ...       | ...    | ...     | ...    | ...      |

## 3.2 Building the Agent

Simply execute the following command from a bash terminal in the same folder as this [README] to build the agent. This will build the Fenland-Trajectory-Agent local Docker Image. 

```
bash ./stack.sh build
```

In case of time out issues in automatically building the StackClients resource indicated in agent building logs, please try pulling the required stack-clients image first by execute `docker pull ghcr.io/cambridge-cares/stack-client:1.27.2` in the terminal before building the agent.

## 3.3 Deploying the Agent
### 1) Configuration and Adding Services

Please place [FenlandTrajectoryAgent.json] in the [stack manager configuration service directory]. Additionally, create a folder named `fta-input` under the stack manager's [data directory] to store all your GPS trajectory data. This operation ensures that any new or updated data in the fta-input folder is automatically available to the agent. Then, ensure that the fenland-trajectory-agent service is included in your stack configuration files normally located in the [stack manager configuration directory] by adding the following to your stack configuration JSON file:

```
{
    "services": {
        "includes": [
            "fenland-trajectory-agent"
        ]
    },
    "volumes": {
        "fta-input": "fta-input"
    }
}

```

### 2) Starting with the Stack Manager
To spin up the stack, both a `postgis_password` and `geoserver_password` file need to be created in the `Deploy/stacks/dynamic/stack-manager/inputs/secrets/` directory (see detailed guidance in the [Stack Manager README]).
To spin up the stack, create `postgis_password` and `geoserver_password` files in the Deploy/stacks/dynamic/stack-manager/inputs/secrets/ directory. Detailed guidance can be found in the [Stack Manager README]. Thereafter，navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal, Once the stack is up, the agent can be started within the stack autimatically, and will be available in the stack's drop-down container menu.

```

./stack.sh start <STACK NAME> <PORT>

```

# 4. Using the Agent

## 4.1 Data Ingestion

The agent automatically registers a task at startup to ingest data from the target GPS folder into the Knowledge Graph. This background task is designed to be triggered via HTTP requests after the agent has started. 

### Functionality Description

The operation of data ingestion is streamlined into two key steps: **Preprocess** and **Instantiate**. Here is a brief description of each:

- **Preprocess**:  This initial step involves loading and examining the GPS trajectory files in batch to ensure efficient processing. Each file is checked for essential columns (UTC Date, UTC Time, Speed, Heading, Height, Distance, Latitude, and Longitude). Any missing data or misformatted columns, particularly those related to time, are corrected to conform with the [ISO 8601] standard. During this phase, data is extracted into a  temporary dataframe, allowing for necessary restructuring and corrections without altering the original files.

- **Instantiate**: This further step involves creating geometry classes for the trajectories, generating IRIs, and structuring the data into lists categorized as time-series and non-time-series. These lists serve as inputs for the [TimeSeriesClient] and the [KGClient], respectively. This organized data is then instantiated in both the knowledge graph and the relational database to facilitate queries and analytics.

### Example HTTP Requests
Example requests for preprocessing and instantiating data are available in detailed HTTP files. You can access these examples at the [preprocess] file, the [instantiate] file, and the [layer_generator] file. 

Additionally, services above can be triggered using CURL from a bash terminal. An example CURL command used to load the GPS trajectory files is displayed in [CURL commands folder]. 

## 4.2 Trip Detection

After data instantiation, the agent provides a trip detection route that analyzes GPS trajectories to identify distinct trips and visits. Using Kernel Density Estimation (KDE) and watershed algorithms, the agent processes each trajectory to detect significant locations (hotspots) and movement patterns. The results are stored by updating the "Trip index" column in the timeseries table, where each continuous sequence of GPS points is assigned a unique trip identifier. This functionality is particularly useful for analyzing movement patterns and segmenting long trajectories into meaningful trips. The agent can process multiple trips within a single trajectory and merge the results together, providing a comprehensive view of an individual's movement patterns. For subsequent exposure calculations, the agent focuses exclusively on the detected trips (excluding visits and gaps), and will aggregate the exposure measurements across all trips within the same trajectory to provide a consolidated result. Example requests for trip detection can be found in the [trip_detection] file.

## 4.3 Count-Based Exposure Calculation

The agent is configured to calculate exposure by listening to trajectory IRIs and environmental feature IRIs along with a specified exposure radius. 
The calculation can be triggered via an HTTP request after the agent has started, and its principles and operational procedures are outlined as follows. 

### Functionality Description

The workflow begins by generating a buffer zone around the trajectory geometry, with the buffer extending outward from the trajectory line by a specified exposure radius. Environmental features, such as food retail locations or greenspaces, are then identified and counted based on their intersections with this buffer zone, providing a quantitative measure of exposure. To accommodate different data storage scenarios, the agent provides two routes for exposure calculations:

- **Exposure_count**:  
This route can operate when the environmental data and the Fenland Trajectory Agent are deployed in different stacks. You need to manually configure the [config.properties] file:

1. For the environmental data endpoint (`ENV_DATA_ENDPOINT_URL`):
   - If the TWA stack containing environmental data is deployed on the same machine with different port: use `http://host.docker.internal:<port>/ontop/ui/sparql`
   - If the TWA stack is deployed on an external server: use `http://<server-ip>:<port>/ontop/ui/sparql`

2. For trajectory database configuration:
   - If your trajectory data is managed by the same stack as this agent:
     ```
     TRAJECTORY_DB_HOST=
     TRAJECTORY_DB_PASSWORD=
     ```
     (Just leave these fields empty)
   - If managed by a TWA stack on a different server:
     ```
     TRAJECTORY_DB_HOST=<server-ip>
     TRAJECTORY_DB_PASSWORD=<database-password>
     ```

In this project, we use Ontop SPARQL endpoint as `ENV_DATA_ENDPOINT_URL` in the [config.properties] file to handle data such as Food Retail and Greenspace. In this context, the matching [OBDA mappings] should be configured in advance by [Stack-data-uploader]. Once these settings are complete, the agent will fetch the geometry data for both environmental features and trajectories from the specified endpoint and database, temporarily store the data in a dataframe, and perform the calculations within the agent.

A typical example involves using an Ontop SPARQL endpoint as `ENV_DATA_ENDPOINT_URL`, where this Ontop endpoint can originate from any other stack created by the [Stack Manager]. Food Retail and Greenspace datasets can be used as demonstration cases. When uploading these structured datasets to the stack, the [Stack-data-uploader] should be preconfigured with particular [OBDA mapping] files containing key metadata, such as FeatureType and SourceTableName. These mappings are essential, as the exposure calculation relies on this metadata to trigger the appropriate computation methods.

- **Exposure_count_single_stack**: If the environmental data and the Fenland Trajectory Agent are deployed in the same stack.
This route is used when the environmental data and the Fenland Trajectory Agent are deployed in the same stack. In this case, the agent will query the Ontop SPARQL endpoint deployed within the same stack to fetch the datasource table name corresponding to the trajectory IRI. The agent will then populate an query and execute the calculation internally within the stack.

### Example HTTP Requests
Example requests are available in detailed HTTP files. You can access these examples at the [exposure_count] file and the [exposure_count_single_stack] file. Services can also be triggered using CURL from a bash terminal. Examples are displayed in [CURL commands folder]. 

&nbsp;
# Authors
Jiying Chen (jc2341@cam.ac.uk), Jan 2025
 

<!-- Links -->
<!-- websites -->
[OntoDevice]: https://www.theworldavatar.com/kg/ontodevice
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[Stack Manager README]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[JPS_Document]: https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-pydantic-rdflib/JPS_BASE_LIB/python_wrapper/docs/examples/additional_java_lib.md
[twa]: https://pypi.org/project/twa/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[Python wrapper]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper#installing-additional-java-resources
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[DataPoint]: https://www.metoffice.gov.uk/services/data/datapoint/about
[Github container registry]: https://ghcr.io
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[http://localhost:5000/]: http://localhost:5000/
[Java Development Kit version >=11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[JDBC driver]: https://jdbc.postgresql.org/download/ 
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[ISO 8601]: https://www.iso.org/iso-8601-date-and-time-format.html
[knowledge graph operations guidance]: https://cambridge-cares.github.io/TheWorldAvatar/examples/sparql/
[TWA-VF]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework
[Stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[AI-for-Public-Health]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/AI4PublicHealth
[HTTP POST]: https://www.w3schools.com/tags/ref_httpmethods.asp
[CURL]: https://learn.microsoft.com/en-us/dynamics365/business-central/dev-itpro/developer/devenv-web-client-urls

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[gps_target_folder]: ./agent/raw_data/gps_target_folder
[resources]: ./resources
[README]: ./README.md
[FenlandTrajectoryAgent.json]: ./stack-manager-input-config-service/
[stack manager configuration service directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[stack manager configuration directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/
[data directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data
[CURL commands folder]: ./example-requests/curl
[SendHTTP]: ./example-requests/SendHTTP
[preprocess]: ./example-requests/SendHTTP/gps_preprocess.http
[instantiate]: ./example-requests/SendHTTP/gps_instantiate.http
[layer_generator]: ./example-requests/SendHTTP/layer_generator.http
[KGClient]: ./agent/kgutils/kgclient.py
[SQL commands for virtual-table generation]: ./agent/layergenerator/virtual_table.sql
[config.properties]:./agent/flaskapp/exposure/config.properties
[exposure_count]: ./example-requests/SendHTTP/exposure_count.http
[exposure_count_single_stack]: ./example-requests/SendHTTP/exposure_count_single_stack.http
[OBDA mapping]: ./resources/ontop.obda
[Input]: ./Input/
[target_gps_folder.env]:./target_gps_folder.env
[trip_detection]:./example-requests/SendHTTP/trip_detection.http