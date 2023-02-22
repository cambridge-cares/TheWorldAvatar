# EPC Instantiation Agent

The `Energy Performance Certificate` (EPC) agent is an input agent which queries data from the [EPC APIs] (i.e. 3 individual endpoints for domestic, non-domestic and display certificates), and instantiates it according to the [OntoBuiltEnv] ontology in the [TheWorldAvatar] knowledge graph.

The agent is designed to interact with the stack spun up by the stack manager. It is implemented as Flask App and uses Celery to run long running requests as background tasks. 

**Please note**:
1) Celery workers are run with superuser privileges to avoid permission issues with accessing particular repositories
2) Tests are currently still excluded.


# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

&nbsp;
## 1.1 Prerequisites

Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):

### **1) The environment variables used by the agent container**


```bash
ENCODED_AUTH          # Base64-encoded authentication token for EPC API
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver layer name, ALSO table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   
ONTOP_FILE            # Path to ontop mapping file (i.e. within Docker container)
OCGML_ENDPOINT        # SPARQL endpoint with instantiated OntoCityGml building instances incl. UPRNs
```

### **2) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to [CARES container registry on Github]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Container registry on Github] simply run the following command to establish the connection and provide the access token when prompted:
```
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

### **3) Accessing CMCL Docker registry**

Building the agent requires the [Stack-Clients] resource from a Docker image published at the CMCL docker registry. In case you don't have credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page.

### **4) VS Code specifics**

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

&nbsp;
## 1.2 Spinning up the core stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. To spin up the stack, both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance in the [Stack Manager] README). There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start <STACK NAME>

# Stop the stack
bash ./stack.sh stop <STACK NAME>

# Remove stack services (incl. volumes)
bash ./stack.sh remove <STACK_NAME> -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [Stack Manager] README.

&nbsp;
## 1.3 Deploying the agent to the stack

This agent requires [JPS_BASE_LIB] and [Stack-Clients] to be wrapped by [py4jps]. To successfully interact with the Java side via py4jps a [Java Runtime Environment version >=11] is required. *Please note, that compiling the Java resources requires a Java Development Kit version >=11. However, updating the [JPS_BASE_LIB] resource is ONLY required if a pre-release version is needed, which is (currently) not the case for this agent.*

Simply execute the following command in the same folder as this `README` to build and spin up the *production version* of the agent (from a bash terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Building the agent Docker image and pushing it
bash ./stack.sh build
# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK NAME>
```

In case of time out issues in automatically building the StackClients resource, please try pulling the required stack-clients image first by `docker pull docker.cmclinnovations.com/stack-client:1.6.2`

The *debug version* will run when built and launched through the provided VS Code `launch.json` configurations:
> **Build and Debug**: Build Debug Docker image (incl. pushing to ghcr.io) and deploy as new container (incl. creation of new `.vscode/port.txt` file)

> **Debug**: Pull Debug Docker image from ghcr.io and deploy as new container (requires deletion of existing `.vscode/port.txt` to ensure mapping to same port)

> **Reattach and Debug**: Simply reattach debugger to running Debug Docker image. In case Debug image needs to be manually started as container, the following command can be used: 
`bash ./stack.sh start TEST-STACK --debug-port <PORT from .vscode/port.txt>`


&nbsp;
## 1.4 Spinning up the Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout main
$ git pull
```
Once the repository clone is obtained, please follow the [Stack Manager] README and instructions above to spin up the stack on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first. Before starting development of the dockerized agent remotely, all required VSCode extensions shall be installed on the remote machine (e.g. *augustocdias.tasks-shell-input* or the *Python extension*).

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

Agent start-up will automatically register a recurring task to assimilate latest EPC data for all instantiated UPRNs every 4 weeks (i.e. new data is published 3-4 times a year). Besides this recurring background task, additional HTTP requests can be sent to the agent. The Blazegraph namespace as specified in the `docker-compose` file is created upon agent startup and the [OntoBuiltEnv] ontology as well as all required unit symbols are uploaded to it (if not already existing).

&nbsp;
## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root http://localhost:5001/epcagent. All requests are to be sent as POST requests and all available endpoints are listed below. Example requests are provided in the [resources] folder.

- POST request to instantiate all postcodes in a given local authority:
> `/epcagent/instantiate/postcodes`

- POST request to instantiate EPC building data for given single certificate:
> `/epcagent/instantiate/certificates/single`

- POST request to instantiate latest EPC building data for all instantiated UPRNs in all instantiated postcodes:
> `/epcagent/instantiate/certificates/all`

- GET request to instantiate/update building footprint and elevation information as instantiated for linked OntoCityGml instance according to OntoBuiltEnv for all buildings:
> `/epcagent/add/ocgml_info`

- GET request to initialise the OntoCityGml knowledge base at specified OCGML_ENDPOINT and upload previously instantiated and exported quads into it (this functionality is mainly kept for reference and the [Stack Data Uploader] should be used instead):
> `/ocgml/initialise`

&nbsp;
# 3. Current EPC data instantiation workflow

The following workflow refers to the state of the agent as of commit `0f647a0348d8bbd682917df5e9671dc3c4f91e51`, but is unlikely to change soon:

### **1) Ensure instantiated OntoCityGml building data is available**
The agent requires an available SPARQL endpoint to retrieve instantiated OntoCityGml building data (i.e. EPC data will only be instantiated for buildings with an OntoCityGml representation). This endpoint is specified as `OCGML_ENDPOINT` in the [docker compose file]. The instantiated data needs to contain a coordinate reference system definition in the following form:
```
PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#> 

<http://<host>:<port>/blazegraph/namespace/<namespace>/sparql/> ocgml:srsname 'EPSG:<EPSG code>' ;
																ocgml:srid '1'^^xsd:integer. 
```

In case the quad data is not already available via a SPARQL endpoint, the following steps can be used:

1) Export OntoCityGml quads from local Blazegraph and unzip file. 
2) Use [Stack Data Uploader] to upload quads (**preferred**) or follow steps below:
3) Rename file to `data.nq` and place it into the `data` folder within this agent.
4) Spin up a remotely hosted Docker container with a Blazegraph image. Using SSH in VSCode (as described in section 1.4), this can be achieved using the provided `./resources/blazegraph_ocgml/docker-compose.yml` file and the following command:
    ```bash
    docker-compose -f "docker-compose.yml" up
    ```
    This shall bring up Blazegraph at endpoint `http://<HOST IP>:4999/blazegraph/`. Please make sure that this matches the provided `OCGML_ENDPOINT` in the [docker compose file]. Alternatively, this endpoint can refer to a new namespace within the Stack's Blazegraph.
5) Send a `GET` request to `/ocgml/initialise` to create the `ocgml` namespace and upload the quad data

### **2) Instantiate relevant postcode instances**

Instantiate all relevant postcodes for provided local authority code by sending `POST` request to `/epcagent/instantiate/postcodes`

### **3) Instantiate all EPC building data**

Instantiate EPC data for all instantiated UPRNs (and postcodes) by sending `POST` request to `/epcagent/instantiate/certificates/all`

### **4) Run Building Matching Agent**

The following steps refer to the Building Matching agent version 1.0.1 as of commit `b2fd81c079956dc184c9329b0ee7bd5e07c42224`. More details can be found in the [Building Matching Readme]:

1) Ensure both SPARQL endpoints, i.e. one containing buildings instantiated in OntoCityGML (`ocgml_endpoint`) and one with their OntoBuiltEnv counterparts (`epc_endpoint`), are available. In the following, let's assume the endpoints are:

    > ocgml endpoint: http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql

    > epc_endpoint: http://165.232.172.16:3838/blazegraph/namespace/buildings/sparql

2) Build and start the agent as Docker container by running the following command within the directory where the [Building Matching Readme] is located. Please note that your github username and access token need to be provided as single-word text files `repo_username.txt` and `repo_password.txt` in the [credentials] folder of the Building Matching agent. (Maybe a `--build` flag needs to be added to the docker compose command to force agent rebuild in case a previous version has been used before.)
    ```bash
    docker-compose up -d
    # To force rebuilding of image before deploying
    docker-compose up --build -d
    ```

3) Once the agent is available at its endpoint `http://localhost:58085/BuildingMatchingAgent/match`, it accepts PUT requests with a JSON object as follows (an [example request] to match the building instances in the previously introduced SPARQL endpoints is also provided):
    ```json
    { 
      "ocgml": "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql",
      "obe": "http://165.232.172.16:3838/blazegraph/namespace/buildings/sparql",
      "prefixIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
    }
    ```

### **5) Update geospatial representation of buildings in OntoBuiltEnv namespace**

To allow for visualisation using the [Digital Twin Visualisation Framework], the geospatial representation of the buildings (i.e. their 2D footprints) needs to be uploaded to PostGIS using the Stack architecture. Running a GET request to the `/epcagent/add/ocgml_info` endpoint, queries the building footprints as well as elevations and instantiates them as required by OntoBuiltEnv.


&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), February 2023


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Container registry on Github]: http://ghcr.io
[CARES container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[EPC APIs]: https://epc.opendatacommunities.org/docs/api
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[http://localhost:5001/]: http://localhost:5001/
[Java Runtime Environment version >=11]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[JDBC driver]: https://jdbc.postgresql.org/download/ 
[OntoBuiltEnv]: https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontobuiltenv/OntoBuiltEnv.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh

<!-- github -->
[Building Matching Readme]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/BuildingMatchingAgent/README.md
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[credentials]: https://github.com/cambridge-cares/TheWorldAvatar/tree/1376-dev-building-matching-agent/Agents/BuildingMatchingAgent/credentials
[Digital Twin Visualisation Framework]:https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack Data Uploader]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-data-uploader/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[CMCL Docker registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[resources]: ./resources
[stack.sh]: ./stack.sh
[example request]: ./resources/matching_agent/HTTPRequest_MatchingAgent.http
