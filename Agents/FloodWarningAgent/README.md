# Flood Warning Instantiation Agent

The `FloodWarningAgent` is an input agent which queries data from the UK [Environment Agency Real Time flood-monitoring API] and instantiates it according to the [OntoFlood] ontology in the [TheWorldAvatar] knowledge graph. The Environment Agency issues warnings of floods that cover specific warning or alert areas. The floods API provides a listing of all current such warnings and is updated every 15 minutes. The agent assimilates new flood warning and alert data hourly, and can furthermore be called manually.

The agent is implemented as Docker container to be deployed to a Docker stack spun up by the [Stack Manager].

&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

## 1.1 Prerequisites

Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (details and defaults are provided below):

### **1) Environment variables to be set in docker-compose file**

```bash
# Stack & Stack Clients configuration
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`, i.e. required for Ontop to access database)
LAYERNAME             # Geoserver layer name, ALSO table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   
ONTOP_FILE            # Path to ontop mapping file (i.e. path within Docker container)
```

### **2) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to the [Github package repository]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the Github container registry simply run the following command to establish the connection and provide the access token when prompted:
```
docker login ghcr.io -u <github_username>
<github_personal_access_token>
```

### **3) Accessing CMCL docker registry**

The agent requires building the [Stack-Clients] resource from a Docker image published at the CMCL docker registry. In case you don't have credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page.

### **4) VS Code specifics**

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

&nbsp;
## 1.2 Spinning up the stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. To spin up the stack, both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (for details see the [Stack Manager] README). There are several common stack scripts provided to manage the stack:

```bash
# Start the stack (please note that this might take some time) - the port is optional and defaults to 3838
bash ./stack.sh start <STACK_NAME> <PORT>

# Stop the stack
bash ./stack.sh stop <STACK_NAME>

# Remove stack services (incl. volumes)
bash ./stack.sh remove <STACK_NAME> -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [Stack Manager] README.

&nbsp;
## 1.3 Deploying the agent to the stack

Simply execute the following command in the same folder as this `README` to spin up the *production version* of the agent (from a *bash* terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Building the agent Docker image and pushing it ghcr.io
bash ./stack.sh build

# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK_NAME>
```

In case of time out issues in automatically building the StackClients resource, please try pulling the required stack-clients image first by `docker pull docker.cmclinnovations.com/stack-client:1.6.2`

The *debug version* of the agent can be launched through the provided VS Code `launch.json` configurations:
> **Build and Debug**: Build Debug Docker image (incl. pushing to [Github package repository]) and deploy as new container (incl. creation of new `.vscode/port.txt` file)

> **Debug**: Pull Debug Docker image from [Github package repository] and deploy as new container (requires deletion of existing `.vscode/port.txt` to ensure mapping to same port)

> **Reattach and Debug**: Simply reattach debugger to running Debug Docker image. In case Debug image needs to be manually started as container, the following command can be used: 
`bash ./stack.sh start <STACK_NAME> --debug-port <PORT from .vscode/port.txt>`

&nbsp;
## 1.4 Observed issues when deploying the agent

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

Agent start-up will automatically register a recurring task to assimilate latest flood alerts and warning on an hourly basis in the background. Besides this recurring background task, additional HTTP requests can be sent (but might be delayed) to the agent. An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root http://localhost:5007/floodwarnings.

- GET request to update all floos warnings and alerts (i.e. instantiate missing flood areas and flood warnings, update instantiated flood warnings, and delete obsolete flood warnings)
> `/floodwarnings/update/all` 

Example requests are provided in the [resources] folder.



&nbsp;
# Authors
Markus Hofmeister (mh807@cam.ac.uk), February 2023


<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[CMCL Docker registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description


[Environment Agency Real Time flood-monitoring API]: https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-warnings
[OntoFlood]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoflood
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-clients
[Stack Manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar

<!-- Files -->
[Docker compose file]: docker-compose.yml
[Dockerfile]: Dockerfile
[resources]: resources
