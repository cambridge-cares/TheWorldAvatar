# Description

The `MetOffice` agent is an input (and output) agent which queries data from the MetOffice API, also known as [DataPoint], and instantiates it according to the [OntoEMS] ontology in the [TheWorldAvatar] knowledge graph.

The agent is implemented as Docker container to be deployed to a Docker stack spun up by the [Stack Manager]. 


&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

## 1.1 Prerequisites

Retrieving data from the MetOffice DataPoint API requires registration for the [DataPoint] platform. Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):

### **1) The environment variables used by the agent container**

```bash
# Agent configuration
API_KEY               # MetOffice DataPoint API key
# Stack & Stack Clients configuration
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver ayer name, ALSO table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   
ONTOP_FILE            # Path to ontop mapping file (i.e. within Docker container)
```


### **2) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to the [Github container registry]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Github container registry] simply run the following command to establish the connection and provide the access token when prompted:
```
docker login ghcr.io -u <github_username>
<github_personal_access_token>
```

### **3) Accessing CMCL docker registry**

The agent requires building the [StackClients] resource from a Docker image published at the CMCL docker registry. In case you don't have credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page.

### **4) VS Code specifics**

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

&nbsp;
## 1.2 Spinning up the stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. To [spin up the stack], both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance following the provided link). There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time) - the port is optional and defaults to 3838
bash ./stack.sh start <STACK_NAME> <PORT>

# Stop the stack
bash ./stack.sh stop <STACK_NAME>

# Remove stack services (incl. volumes)
bash ./stack.sh remove <STACK_NAME> -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [spin up the stack] readme.

&nbsp;
## 1.3 Deploying the agent to the stack

This agent requires [JPS_BASE_LIB] to be wrapped by [py4jps]. Please note, that compiling requires a [Java Development Kit version >=11]. *Updating the [JPS_BASE_LIB] resource is ONLY required if a pre-release version is needed, which is (currently) not the case for this agent.*

Simply execute the following command in the same folder as this `README` to spin up the *production version* of the agent (from a *bash* terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Buildings the agent Docker image and pushing it
bash ./stack.sh build

# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK_NAME>
```

In case of time out issues in automatically building the StackClients resource, please try pulling the required stack-clients image first by `docker pull docker.cmclinnovations.com/stack-client:1.6.2`

The *debug version* will run when built and launched through the provided VS Code `launch.json` configurations:
> **Build and Debug**: Build Debug Docker image (incl. pushing to [Github container registry]) and deploy as new container (incl. creation of new `.vscode/port.txt` file)

> **Debug**: Pull Debug Docker image from [Github container registry] and deploy as new container (requires deletion of existing `.vscode/port.txt` to ensure mapping to same port)

> **Reattach and Debug**: Simply reattach debugger to running Debug Docker image. In case Debug image needs to be manually started as container, the following command can be used: 
`bash ./stack.sh start <STACK_NAME> --debug-port <PORT from .vscode/port.txt>`

> **Update JPSRM and Build and Debug**: Updated StackClient py4jps resource and build the Debug Docker image (incl. pushing to ghcr.io) and deploy it as new container (incl. creation of new `.vscode/port.txt` file)


&nbsp;
## 1.4 Spinning up the Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentication.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO_NAME>
$ cd <REPO_NAME>
$ git checkout main
$ git pull
```
Once the repository clone is obtained, please follow these instructions to [spin up the stack] on the remote machine (also detailed and referenced above). In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

Before starting development or spinning up the dockerized agent remotely, all required VSCode extensions shall be installed on the remote machine (e.g. *augustocdias.tasks-shell-input* or the *Python extension*).

```bash
# Ensure Java Development Kit version >=11 is available
# Test installation
java -version
javac -verison
# Install in case it is missing
sudo apt install openjdk-11-jdk-headless

# Ensure MAVEN is available
# Test installation
mvn -version
# Install in case it is missing
sudo apt install maven
```
To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

```bash
# Check permissions
ls -l <REPO_NAME>
# Grant (all) permissions
chmod -R +rwx <REPO_NAME>
# To prevent git from identifying all files as changed (due to changed permission rights), 
# i.e. exclude file permission (chmod) changes from git 
git config core.fileMode false
```


&nbsp;
# 2. Using the Agent

The provided [Dockerfile] contains instructions to create Docker images for both the Debugging and Production stage. The debugging image allows for hot-reloading code changes by mounting the `agent` folder containing the source code as external volume. While the production image starts the agent immediately after the container has started, the debugging image awaits for the external debugger to connect before starting the agent. 

## Provided functionality

Agent start-up will automatically register a recurring task to assimilate latest weather data into the KG, i.e. MetOffice observations and forecasts once per day. Besides those recurring background tasks, additional HTTP requests can be sent (but they might be delayed) to the agent. An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5000/]. All requests are to be sent as GET requests and all available endpoints are listed below:

- GET request to instantiate all Met Office stations (only new stations will be added, already instantiated stations will not be overwritten)
> `/api/metofficeagent/instantiate/stations` 
- GET request to instantiate Met Office readings for instantiated stations (only new station readings will be added, already instantiated readings will not be overwritten)
> `/api/metofficeagent/instantiate/readings`
- GET request to add latest time series readings for all instantiated time series 
> `/api/metofficeagent/update/timeseries`
- GET request to update all stations and associated readings, and add latest data for all time series (i.e. instantiate missing stations and readings and append latest time series readings)
> `/api/metofficeagent/update/all`
- GET request to retrieve data about Met Office stations and create respective output files for DTVF (i.e. request expects all individual query parameter to be provided in a single nested JSON object with key 'query')
> `/api/metofficeagent/retrieve/all`

Example requests are provided in the [resources] folder. The [example retrieve all request] contains further information about allowed parameters to query station and readings data from the knowledge graph and create the respective output files.


&nbsp;
# 3. Agent Tests

Several unit and integration tests are provided in the [tests] repository. Although the agent is designed to work within the stack, those tests *only* test for correct functionality of the locally deployed agent together with Blazegraph and PostgreSQL spun up as Docker containers. Interactions with the stack (e.g. retrieval of settings and endpoints, uploading geospatial data to PostGIS) are simply "mocked" and not tested.

To run the integration tests locally, access to the CMCL's Docker registry is required on the local machine (for more information regarding the registry, see the [CMCL Docker registry wiki page]). It needs to be noted, that several integration tests are currently commented out as they required both Blazegraph and PostgreSQL to be running. To run those tests, the provided [docker-compose.test.yml] file can be used to spin up these services at the specified endpoints before uncommenting them. 

To run the tests, please follow those instructions:


1. It is highly recommended to use a virtual environment for testing. The virtual environment can be created as follows:
    ```bash
    $ python -m venv metoffice_venv
    $ metoffice_venv\Scripts\activate.bat
    (metoffice_venv) $
    ```
2. Install all required packages in virtual environment (the `-e` flag installs the project for-in place development):
    ```bash
    $ python -m pip install --upgrade pip
    # Install all required packages from setup.py, incl. pytest etc.
    python -m pip install -e .[dev]
    # Install agentlogging (separate installation required, as not possible to include in setup.py)
    python -m pip install -r requirements.txt
    ```
3. Run integration tests with agent deployed locally (i.e. in memory) and Blazegraph and PostgreSQL spun up as Docker containers:
    ```bash
    # Uncomment integration tests and start Docker services (if wanted)
    docker compose -f "tests\docker-compose.test.yml" up -d --build 
    # Run tests
    pytest
   ```


&nbsp;
# Authors
Markus Hofmeister (mh807@cam.ac.uk), October 2022

(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com))


<!-- Links -->
<!-- websites -->
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
[OntoEMS]: http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[StackClients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-clients
[CMCL Docker registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

<!-- files -->
[Dockerfile]: Dockerfile
[docker compose file]: docker-compose.yml
[docker-compose.test.yml]: tests\docker-compose.test.yml
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http
[resources]: resources
[stack.sh]: stack.sh
[tests]: tests
