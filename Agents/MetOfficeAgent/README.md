# Description

The `MetOffice` agent is an input (and output) agent which queries data from the MetOffice API, also known as [DataPoint], and instantiates it according to the [OntoEMS] ontology in the [TheWorldAvatar] knowledge graph.

It is designed to interact with the stack spun up by the stack manager. 

<span style="color:red">Tests are currently still excluded and have not been updated to work with the stack architecture yet.</span>

# Setup

This section specifies the minimum requirement to build the docker image. 

## Prerequisites

Retrieving data from the MetOffice DataPoint API requires registration for the [DataPoint] platform. Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):

### **1) The environment variables used by the agent container**

1) STACK_NAME
2) API_KEY (MetOffice DataPoint API key)
3) LAYERNAME
4) DATABASE
5) GEOSERVER_WORKSPACE

### **2) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to the [Container registry on Github]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Container registry on Github] simply run the following command to establish the connection and provide the access token when prompted:
```
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

### **3) VS Code specifics**

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

## Spinning up the stack

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

## Deploying the agent to the stack

This agent requires [JPS_BASE_LIB] and [Stack-Clients] to be wrapped by [py4jps]. Therefore, after installation of all required packages (incl. `py4jps`), its `JpsBaseLib` resource might need to get updated and the `StackClients` resource needs to be added to allow for access through `py4jps`. The required steps are detailed in the [py4jps] documentation and already included in the respective [stack.sh] script and [Dockerfile]. Compiling those resources requires a [Java Runtime Environment version >=11].

Simply execute the following command in the same folder as this `README` to build and spin up the *production version* of the agent (from a bash terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Compiling latest py4jps resources (JPS_BASE_LIB, Stack_Clients)
build_py4jps_resources.sh
# Deploying the agent
./stack.sh start <STACK NAME>
```

The *debug version* will run when built and launched through the provided VS Code `launch.json` configurations:
> **Build and Debug**: Build Debug Docker image (incl. pushing to ghcr.io) and deploy as new container (incl. creation of new `.vscode/port.txt` file)

> **Debug**: Pull Debug Docker image from ghcr.io and deploy as new container (requires deletion of existing `.vscode/port.txt` to ensure mapping to same port)

> **Reattach and Debug**: Simply reattach debugger to running Debug Docker image. In case Debug image needs to be manually started as container, the following command can be used: 
`bash ./stack.sh start TEST-STACK --debug-port <PORT from .vscode/port.txt>`

> **Update JPSRM and Build and Debug**: Updated py4jps resources and builds the Debug Docker image (incl. pushing to ghcr.io) and deploys it as new container (incl. creation of new `.vscode/port.txt` file)

## Spinning up the Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout dev-MetOfficeAgent-withinStack
$ git pull
```

Once the repository clone is obtained, please follow these instructions to [spin up the stack] on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

# How to use the Agent

The provided `docker-compose` file contains instructions to create Docker images for both the Debugging and Production stage. The debugging image allows for hot-reloading code changes by mounting the `metoffice` folder containing the source code as external volume. While the production image starts the agent immediately after the container has started, the debugging image awaits for the external debugger to connect before starting the agent. 

## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5000/]. All requests are to be sent as GET requests and all available endpoints are listed below:

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

Example requests are provided in the [resources] folder. The [example retrieve all request] contains further information about allowed parameters to query station and readings data from the knowledge graph and create the respective output files. It has to be noted that using the `circleCenter` and `circleRadius` parameters to retrieve only stations within a particular area (using Blazegraph's geospatial search capabilities) requires a Blazegraph namespace with geospatial capabilities enabled.

Agent start-up will automatically register recurring tasks to assimilate latest time series data (i.e. every hour) and to create DTVF output files (i.e. once per day). Besides those recurring background tasks, additional HTTP requests can be sent (but they might be delayed) to the agent.

# Authors #
Markus Hofmeister (mh807@cam.ac.uk), March 2022

(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com))


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[DataPoint]: https://www.metoffice.gov.uk/services/data/datapoint/about
[Container registry on Github]: ghcr.io
[http://localhost:5000/]: http://localhost:5000/
[Java Runtime Environment version >=11]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[OntoEMS]: http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh

<!-- files -->
[Dockerfile]: Dockerfile
[docker compose file]: docker-compose.yml
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http
[resources]: resources
[stack.sh]: stack.sh