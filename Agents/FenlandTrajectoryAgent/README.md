# 1. Description

The `Fenland Trajectory Agent` is a specialized tool for batch instantiation of structured time-series GPS trajectory data. This agent is capable of receiving HTTP POST requests or CURL commands to load GPS trajectory files, which it subsequently instantiates into triples before uploading them into the knowledge graph.

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

## Functionality Description

The operation of this agent is streamlined into two key steps: **Preprocess** and **Instantiate**. Here is a brief description of each:

- **Preprocess**:  This initial step involves loading and examining the GPS trajectory files in batch to ensure efficient processing. Each file is checked for essential columns (UTC Date, UTC Time, Speed, Heading, Height, Distance, Latitude, and Longitude). Any missing data or misformatted columns, particularly those related to time, are corrected to conform with the [ISO 8601] standard. During this phase, data is extracted into a  temporary dataframe, allowing for necessary restructuring and corrections without altering the original files.

- **Instantiate**: This further step involves creating geometry classes for the trajectories, generating IRIs, and structuring the data into lists categorized as time-series and non-time-series. These lists serve as inputs for the [TimeSeriesClient] and the [KGClient], respectively. This organized data is then instantiated in both the knowledge graph and the relational database to facilitate queries and analytics.

Importantly, the preprocessing step must be completed before moving on to the instantiation step. This sequence is crucial as it ensures that all data is properly formatted and organized, making it ready for use in the knowledge graph and the relational database.

In addition to the core functionality, this agent offers a route task called layer_generator that creates GeoServer layers from PostGIS tables for visualisation. Given a table name (formed by UUID) and latitude/longitude column names, the agent deploys the [SQL commands for virtual-table generation], creates vector objects, and communicates with the GeoServer REST API to publish the layer. Please note that this functionality is specialized and typically requires manual examination of SQL commands and layer metadata (such as EPSG) to ensure compatibility with specific requirements. This functionality will be further updated and refined after the AI for Public Health (Fenland) project's visualisation is switched to [TWA-VF] 5.4.0.

## Example HTTP Requests
Example HTTP requests for preprocessing and instantiating data are available in detailed HTTP files. You can access these examples at the [preprocess] file, the [instantiate] file, and the [layer_generator] file. 

Additionally, services above can be triggered using Client URL (CURL) from a bash terminal. An example CURL command used to load the GPS trajectory files is displayed in [CURL commands folder]. 

# 5. Jupyter Notebook

A Jupyter notebook is provided in [/notebook](./notebook/) to demonstrate the following functionalities:
- Fetch environmental features
- Fetch trajectory data
- Process trajectory
- Exposure calculation on area features
- Exposure calculation on point feature

## Configuration
User is required to provide the configuration of knowledge graph and database in [config.env](./notebook/config.env). The complete setup guide can be found in the [notebook](./notebook/environment_exposure.ipynb).

## Docker Deployment
User is encouraged to use docker to launch the notebook, where the requirements are pre-installed in the container. However, the `config.env` file and `templates/` folder are bind-mount, allowing any changes made to be reflected immediately inside the container.

Execute the following command in [/notebook](./notebook/) to launch the jupyter notebook.
```
docker compose up --build
```

## Binder Deployment
Binder suggests [not to build with customized Dockerfile](https://mybinder.readthedocs.io/en/latest/tutorials/dockerfile.html#use-a-dockerfile-for-your-binder-repository) but use `requirements.txt` for [python package installation](https://mybinder.readthedocs.io/en/latest/examples/sample_repos.html#python-environment-with-a-requirements-txt) and `apt.txt` for [apt packages](https://mybinder.readthedocs.io/en/latest/examples/sample_repos.html#installing-packages-from-apt-repositories). 

The notebook uses [`twa` python package](https://pypi.org/project/twa/), which requires `openjdk-17-jdk-headless` to be installed. This has been included in the pre-configured `apt.txt`. And the required python packages has been added to `requirements.txt` as well. 

The notebook is currently **not deployed** on Binder because
1. Binder reduced their computation capacity due to funding issue. Services deployed with Binder is not stable.
2. Binder restricts access to all ports except 80 (HTTP) and 443 (HTTPS). The notebook fails to access the TWA stack because it is deployed on other ports and doesn't have HTTPS setup.


> [!IMPORTANT]  
> Binder restricts access to all ports except 80 (HTTP) and 443 (HTTPS). The TWA stack used by the notebook should have Nginx deployed on these ports.

> [!IMPORTANT]  
> Binder requires a repository link to build the notebook image, and both requirements.txt and apt.txt must be located in the root directory of the repository. To deploy the notebook using Binder, create a separate repository and copy the contents of the [notebook](./notebook/) folder into it.


&nbsp;
# Authors
Jiying Chen (jc2341@cam.ac.uk), May 2024
 

<!-- Links -->
<!-- websites -->
[OntoDevice]: https://www.theworldavatar.com/kg/ontodevice
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[TimeSeriesClient]: https://github.com/TheWorldAvatar/baselib/tree/main/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Stack Manager]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager
[Stack Manager README]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/README.md
[JPS_Document]: https://github.com/TheWorldAvatar/baselib/tree/main/python_wrapper/docs/examples/additional_java_lib.md
[twa]: https://pypi.org/project/twa/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[Python wrapper]: https://github.com/TheWorldAvatar/baselib/tree/main/python_wrapper#installing-additional-java-resources
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[Common stack scripts]: https://github.com/TheWorldAvatar/stack/tree/main/common-scripts
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[DataPoint]: https://www.metoffice.gov.uk/services/data/datapoint/about
[Github container registry]: https://ghcr.io
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[http://localhost:5000/]: http://localhost:5000/
[Java Development Kit version >=11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[JDBC driver]: https://jdbc.postgresql.org/download/ 
[JPS_BASE_LIB]: https://github.com/TheWorldAvatar/baselib/tree/main
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Stack Manager]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager
[spin up the stack]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/README.md
[Stack-Clients]: https://github.com/TheWorldAvatar/stack/tree/main/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[ISO 8601]: https://www.iso.org/iso-8601-date-and-time-format.html
[knowledge graph operations guidance]: https://cambridge-cares.github.io/TheWorldAvatar/examples/sparql/
[TWA-VF]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[gps_target_folder]: ./agent/raw_data/gps_target_folder
[resources]: ./resources
[README]: ./README.md
[FenlandTrajectoryAgent.json]: ./stack-manager-input-config-service/
[stack manager configuration service directory]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/inputs/config/services
[stack manager configuration directory]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/inputs/config/
[CURL commands folder]: ./example-requests/curl
[SendHTTP]: ./example-requests/SendHTTP
[preprocess]: ./example-requests/SendHTTP/gps_preprocess.http
[instantiate]: ./example-requests/SendHTTP/gps_instantiate.http
[layer_generator]: ./example-requests/SendHTTP/layer_generator.http
[KGClient]: ./agent/kgutils/kgclient.py
[SQL commands for virtual-table generation]: ./agent/layergenerator/virtual_table.sql
