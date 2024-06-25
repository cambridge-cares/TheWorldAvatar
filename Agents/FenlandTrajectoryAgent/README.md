# 1. Description

The `Fenland Trajectory Agent` is a specialized tool for batch instantiation of structured time-series GPS trajectory data. This agent is capable of receiving HTTP POST requests or CURL commands to load GPS trajectory files, which it subsequently instantiates into triples before uploading them into the knowledge graphs.

Presently, the agent focuses on data from the Fenland Study to analyze the interaction between GPS trajectories and environmental features within the context of a digital twin. Example data can be found at Dropbox/CoMo_shared/_Projects/c4e-AI-for-public-health/sample_gps_data.csv. By default, the data instantiated from the Fenland Study using this agent encompasses Speed, Height, Distance, Heading, Latitude, and Longitude. This method is also applicable to other categories of time-series structured data in Fenland Study by replacing or adding the relevant column names. The information instantiated into the knowledge graph adheres to the Ontology of Devices [OntoDevice] in [TheWorldAvatar] project. The instantiation process is executed based on the [TimeSeriesClient]. This agent is implemented as a Docker container, designed for deployment within a stack managed by the [Stack Manager]


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

## 3.1 Placing the GPS Data

The GPS trajectory data is structured and stored in tables (in .csv format). Each table consists of a series of record points, with each point containing eight essential pieces of information for instantiation, including UTC Date, UTC Time, Longitude (degrees), Latitude (degrees), Speed (km/h), Heading (degrees), Height (meters), and Distance (meters). Please place GPS files in the [gps_target_folder]. Below is an example of the columns and values in GPS trajectory tables for instantiation:

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

Please place [FenlandTrajectoryAgent.json] in [stack manager configuration service directory]. Afterward, ensure that the 'fenland-trajectory-agent service' is included in your stack configuration files normally placed in [stack manager configuration directory]

### 2) Starting with the Stack Manager
To spin up the stack, both a `postgis_password` and `geoserver_password` file need to be created in the `Deploy/stacks/dynamic/stack-manager/inputs/secrets/` directory (see detailed guidance in the [Stack Manager README]).
To spin up the stack, create `postgis_password` and `geoserver_password` files in the Deploy/stacks/dynamic/stack-manager/inputs/secrets/ directory. Detailed guidance can be found in the [Stack Manager README]. Thereafter，navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal, Once the stack is up, the agent can be started within the stack autimatically, and will be available in the stack's drop-down container menu.

```

./stack.sh start <STACK NAME> <PORT>

```

# 4. Using the Agent

The agent will automatically register a task upon startup to assimilate the data from the target GPS folder into the Knowledge Graph (KG). This background task can be triggered via an HTTP request after the agent has started, accessible at http://localhost:{Port}/fenland-trajectory-agent. Please replace {Port} with the numerical value of the port on which your stack is running.

## Provided functionality

The files for example http request are displayed at [SendHTTP] folder.

<center>-------- Data Instantiation ------------</center>

- Post request to load and preprocess all GPS trajectory files 
> ./gpstasks/fenlandtrajectoryagent/load_and_preprocess

- Post request to instantiate all GPS trajectory data to KG
> ./gpstasks/fenlandtrajectoryagent/process_and_instantiate

Additionally, services above can be triggered using Client URL (CURL) from a bash terminal. This method provides a straightforward and scriptable way to interact with the agent. An example CURL command used to load the GPS trajectory files is displayed in [CURL commands folder]. 

&nbsp;
# Authors
Jiying Chen (jc2341@cam.ac.uk), May 2024
 

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

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[gps_target_folder]: ./agent/raw_data/gps_target_folder
[resources]: ./resources
[README]: ./README.md
[FenlandTrajectoryAgent.json]: ./stack-manager-input-config-service/
[stack manager configuration service directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[stack manager configuration directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/
[CURL commands folder]: ./example-requests/curl
[SendHTTP]: ./example-requests/SendHTTTP/


