# EPC Instantiation Agent

The `Energy Performance Certificate` (EPC) agent is an input agent which queries data from the [EPC APIs] (i.e. 3 individual endpoints for domestic, non-domestic and display certificates), and instantiates it according to the [OntoBuiltEnv] ontology in the [TheWorldAvatar] knowledge graph.

It is designed to interact with the stack spun up by the stack manager. 

<span style="color:red">Tests are currently still excluded.</span>
<br/><br/>
<span style="color:red">The current version of the agent (i.e. commit ef61ae035f7aeb36b692eddd45188ce7231528c1) retrieves data only from the Domestic EPC API endpoint. (to be extended!)</span>

# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

&nbsp;

## 1.1 Prerequisites

Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):

### **1) The environment variables used by the agent container**

1) STACK_NAME
2) API_AUTH (authentication token for EPC API)
3) DATABASE (database name in PostGIS)
4) LAYERNAME (layer name in Geoserver, also the table name for geospatial features in PostGIS)
5) GEOSERVER_WORKSPACE
6) ONTOP_FILE
7) OCGML_ENDPOINT (SPARQL endpoint with instantiated OntoCityGml building instances incl. UPRNs)

### **2) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to the [Container registry on Github]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Container registry on Github] simply run the following command to establish the connection and provide the access token when prompted:
```
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

### **3) VS Code specifics**

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

&nbsp;

## 1.2 Spinning up the core stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. To [spin up the stack], both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance following the provided link). There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start <STACK NAME>

# Stop the stack
bash ./stack.sh stop <STACK NAME>

# Remove stack services (incl. volumes)
bash ./stack.sh remove <STACK_NAME> -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [spin up the stack] readme.

&nbsp;

## 1.3 Deploying the agent to the stack

This agent requires [JPS_BASE_LIB] and [Stack-Clients] to be wrapped by [py4jps]. Therefore, after installation of all required packages (incl. `py4jps`), its `JpsBaseLib` resource might need to get updated and the `StackClients` resource needs to be added to allow for access through `py4jps`. The required steps are detailed in the [py4jps] documentation and already included in the respective [stack.sh] script and [Dockerfile]. Compiling those resources requires a [Java Runtime Environment version >=11].

Simply execute the following command in the same folder as this `README` to build and spin up the *production version* of the agent (from a bash terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Compiling latest py4jps Stack_Clients resources
bash build_py4jps_resource.sh
# Buildings the agent Docker image and pushing it
bash ./stack.sh build
# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK NAME>
```

The *debug version* will run when built and launched through the provided VS Code `launch.json` configurations:
> **Build and Debug**: Build Debug Docker image (incl. pushing to ghcr.io) and deploy as new container (incl. creation of new `.vscode/port.txt` file)

> **Debug**: Pull Debug Docker image from ghcr.io and deploy as new container (requires deletion of existing `.vscode/port.txt` to ensure mapping to same port)

> **Reattach and Debug**: Simply reattach debugger to running Debug Docker image. In case Debug image needs to be manually started as container, the following command can be used: 
`bash ./stack.sh start TEST-STACK --debug-port <PORT from .vscode/port.txt>`

> **Update JPSRM and Build and Debug**: Updated py4jps resources and builds the Debug Docker image (incl. pushing to ghcr.io) and deploys it as new container (incl. creation of new `.vscode/port.txt` file) 

&nbsp;

## 1.4 Spinning up the Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout dev-MetOfficeAgent-withinStack
$ git pull
```
Once the repository clone is obtained, please follow these instructions to [spin up the stack] on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

Before starting development of the dockerized agent remotely, all required VSCode extensions shall be installed on the remote machine (e.g. *augustocdias.tasks-shell-input* or the *Python extension*). As the Docker image requires the[JPS_BASE_LIB] and [Stack-Clients] `.jar` files to be wrapped by [py4jps], they need to be copied over manually to the respective folders as specified in the [Dockerfile] or can be created remotely by running the *Update JPSRM and Build and Debug* Debug Configuration. In order to build these resources, Java and Maven need to be available on the remote machine. In order to pull TWA specific Maven packages from the [Github package repository], `settings.xml` and `settings-security.xml` files need to be copied into Maven's `.m2` folder on the remote machine (typically located at user's root directory)

```bash
# Java >= 11
# Test installation
java -version
javac -verison
# Install in case it is missing
sudo apt install openjdk-11-jdk-headless

# MAVEN 
# Test installation
mvn -version
# Install in case it is missing
sudo apt install maven
```
To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

```bash
# Check permissions
ls -l <REPO NAME>
# Grant permissions
chmod -R +rwx <REPO NAME>
# To prevent git from identifying all files as changed (due to changed permission rights), exclude file permission (chmod) changes from git
git config core.fileMode false
```
&nbsp;

# 2. Using the Agent

Agent start-up will automatically register a recurring task to assimilate latest EPC data for all instantiated UPRNs every 4 weeks (i.e. new data is published 3-4 times a year). Besides this recurring background task, additional HTTP requests can be sent to the agent.

&nbsp;

## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5001/]. All requests are to be sent as POST requests and all available endpoints are listed below. Example requests are provided in the [resources] folder.

- POST request to instantiate all postcodes in a given local authority:
> `/api/epcagent/instantiate/postcodes`

- POST request to instantiate EPC building data for given single certificate:
> `/api/epcagent/instantiate/certificates/single`

- POST request to instantiate latest EPC building data for all instantiated UPRNs in all instantiated postcodes:
> `/api/epcagent/instantiate/certificates/all`

- GET request to instantiate/update building footprint and elevation information as instantiated for linked OntoCityGml instance according to OntoBuiltEnv for all buildings:
> `/api/epcagent/add/ocgml_info`

&nbsp;

# 3. Current EPC data instantiation workflow

The following workflow refers to the state of the agent as of commit 05ce9c5bc20ad306d7ccfe81645c7e51fac06fac.

### **1) Ensure instantiated OntoCityGml building data is available**
The agent requires an available SPARQL endpoint to retrieve instantiated OntoCityGml building data (i.e. EPC data will only be instantiated for buildings with an OntoCityGml representation). This endpoint is specified as `OCGML_ENDPOINT` in the [docker compose file]. The instantiated data needs to contain a coordinate reference system definition in the following form:
```
PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#> 

<http://<host>:<port>/blazegraph/namespace/<namespace>/sparql/> ocgml:srsname 'EPSG:<EPSG code>' ;
																ocgml:srid '1'^^xsd:integer. 
```

In case the quad data is not already available via a SPARQL endpoint, the following steps can be used:

1) Export OntoCityGml quads from local Blazegraph and unzip file. Rename file to `data.nq` and place it into the `data` folder within this agent.
2) Spin up a remotely hosted Docker container with a Blazegraph image. Using SSH in VSCode (as described in section 1.4), this can be achied using the provided `./resources/blazegraph_ocgml/docker-compose.yml` file and the following command:
    ```bash
    docker-compose -f "docker-compose.yml" up
    ```
    This shall bring up Blazegraph at endpoint http://128.199.197.40:4999/blazegraph/ .
3) Send a `GET` request to `/api/ocgml/initialise` to create the `ocgml` namespace and upload the quad data

### **2) Initialise namespace for EPC building data**

1) Create `buildings` namespace and upload TBox and ABox .owl files by sending `POST` request to `/api/epcagent/initialise`
2) Instantiate all relevant postcode instances by sending `POST` request to `/api/epcagent/instantiate/postcodes`

### **3) Instantiate all EPC building data**

Instantiate EPC data for all instantiated UPRNs (and postcodes) send `POST` request to `/api/epcagent/instantiate/certificates/all`

### **4) Run Building Matching Agent**

The following steps refer to the Building Matching agent (on branch `1376-dev-building-matching-agent`) as of commit 092e750c8fa7a4653b0d4b67b5fd3ff2d643dfcb. More details can be found in the [Building Matching Readme]:

1) Ensure both SPARQL endpoints, i.e. one containing buildings instantiated in OntoCityGML (`ocgml_endpoint`) and one with their OntoBuiltEnv counterparts (`epc_endpoint`), are available. In the following, let's assume the endpoints are:

    > ocgml endpoint: http://128.199.197.40:4999/blazegraph/namespace/ocgml/sparql

    > epc_endpoint: http://128.199.197.40:3838/blazegraph/namespace/buildings/sparql

2) Build and start the agent as Docker container by running the following command within the directory where the [Building Matching Readme] is located. Please note that your github username and access token need to be provided as single-word text files `repo_username.txt` and `repo_password.txt` in the [credentials] folder of the Building Matching agent. (Maybe a `--build` flag needs to be added to the docker compose command to force agent rebuild in case a previous version has been used before.)
    ```bash
    docker-compose up -d
    # To force rebuilding of image before deploying
    docker-compose up --build -d
    ```

3) Once the agent is available at its endpoint `http://localhost:58085/BuildingMatchingAgent/match`, it accepts PUT requests with a JSON object as follows:
    ```json
    { "ocgml": "http://128.199.197.40:4999/blazegraph/namespace/ocgml/sparql",
      "obe": "http://128.199.197.40:3838/blazegraph/namespace/buildings/sparql",
      "prefixIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
    }
    ```
An [example request] to match the building instances in the previously introduced SPARQL endpoints is also provided.

### **5) Update geospatial representation of buildings in OntoBuiltEnv namespace**

To allow for visualisation using the [Digital Twin Visualisation Framework], the geospatial representation of the buildings (i.e. their 2D footprints) needs to be uploaded to PostGIS using the Stack architecture. Running a GET request to the `/api/epcagent/add/ocgml_info` endpoint, queries the building footprints as well as elevations and instantiates them as required by OntoBuiltEnv.


&nbsp;

# Authors #
Markus Hofmeister (mh807@cam.ac.uk), September 2022


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Container registry on Github]: ghcr.io
[EPC APIs]: https://epc.opendatacommunities.org/docs/api
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[http://localhost:5001/]: http://localhost:5001/
[Java Runtime Environment version >=11]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[JDBC driver]: https://jdbc.postgresql.org/download/ 
[OntoBuiltEnv]: http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh

<!-- github -->
[Building Matching Readme]: https://github.com/cambridge-cares/TheWorldAvatar/blob/1376-dev-building-matching-agent/Agents/BuildingMatchingAgent/README.md
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[credentials]: https://github.com/cambridge-cares/TheWorldAvatar/tree/1376-dev-building-matching-agent/Agents/BuildingMatchingAgent/credentials
[Digital Twin Visualisation Framework]:https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-dtvf-cesium/web/digital-twin-vis-framework/example-mapbox-vis
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[resources]: ./resources
[stack.sh]: ./stack.sh
[example request]: ./resources/matching_agent/HTTPRequest_MatchingAgent.http