# Description

The `Fenland Trajectory Agent` is dedicated to process time series GPS data stored in the relational database, instantiate this data as triples based on relevant ontologies, and then upload the triples into the knowledge graph.

Currently this agent is focusing on the data provided from Fenland Study to  analyze the interaction between GPS trajectories and environmental features in the digital twin. The data instatiated in the knowledge graph follows the ontology of device (ontodevice) https://www.theworldavatar.com/kg/ontodevice in the https://github.com/cambridge-cares/TheWorldAvatar. 
The agent is implemented as Docker container to be deployed to a Docker stack spun up by the https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager. `

# Environment setup (Optional)

For development and testing reasons, please follow instructions below to get started.

### Virtual environment setup 

It is highly recommended to use a virtual environment (https://docs.python.org/3/tutorial/venv.html). Follow these steps to set up a virtual environment as described in the Python documentation

`(Windows)`

cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $


`(Linux)`
sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $


The above commands will create and activate the virtual environment `<venv_name>` in the current directory.

### Stack Client Settings 
Our Fenland Trajectory Agent requires importing [JPS_BASE_LIB] and [Stack-Clients] from the [py4jps] package. The [py4jps] package, 
now upgraded to [twa] serves as a [Python wrapper] capable of packaging compiled Java code into .jar files for use within Python. 
Currently, [py4jps] by default only packages [JPS_BASE_LIB]. To utilize [Stack-Clients] through py4jps,the [Stack-Clients] resource 
needs to be added and allow for access. This adding procee can be executed automatically by Docker file employing jpsrm commands. 
However, occasionally commands executed within the Docker environment may not successfully package the components due to potential discrepancies between the Java and Python environments or specific configurations within Docker that interfere with the packaging process.
It is highly recommended to manually configure this environment following the instructions provided at this link: https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-pydantic-rdflib/JPS_BASE_LIB/python_wrapper/docs/examples/additional_java_lib.md.
&nbsp;
# 1. Agent Setup

This section outlines the minimum requirements for building and deploying the Docker image.

## 1.1 Prerequisites

### *1) Place the GPS trajectory in the target position*

In this case, the format of the GPS data table is .csv. Each table consists of a series of record points, each point containing six essential pieces of information. They are UTC date, UTC time, longitude, latitude, speed, and the distance between the current record point and the position corresponding to the previous record point. After preparing all files, please place them in `./FenlandTrajectoryAgent/agent/raw_data/gps_target_data
folder.

### *2) The environment variables used by the agent container*
Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):
bash
# Agent configuration

# Stack & Stack Clients configuration
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver ayer name, ALSO table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   
ONTOP_FILE            # Path to ontop mapping file (i.e. within Docker container)

### *3) Accessing Github's Container registry*

While building the Docker image of the agent, it also gets pushed to the [Github container registry]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Github container registry] simply run the following command to establish the connection and provide the access token when prompted:

docker login ghcr.io -u <github_username>
<github_personal_access_token>

### *4) VS Code specifics*

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

&nbsp;
## 1.2 Spinning up the stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a bash terminal. To [spin up the stack], both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance following the provided link). There are several [common stack scripts] provided to manage the stack:

bash
# Start the stack (please note that this might take some time) - the port is optional and defaults to 3838
bash ./stack.sh start <STACK_NAME> <PORT>

# Stop the stack
bash ./stack.sh stop <STACK_NAME>

# Remove stack services (incl. volumes)
bash ./stack.sh remove <STACK_NAME> -v


After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [spin up the stack] readme.

&nbsp;
## 1.3 Deploying the agent to the stack

This agent requires [JPS_BASE_LIB] and [Stack-Clients] to be wrapped by [py4jps]. Therefore, after installation of all required packages (incl. `py4jps >= 1.0.26`), the `StackClients` resource needs to be added to allow for access through `py4jps`. All required steps are detailed in the [py4jps] documentation. However, the commands provided below shall suffice to compile the latest `StackClients` resource locally and install it inside the Docker container using the provided [Dockerfile]. Please note, that compiling requires a [Java Development Kit version >=11]. Updating the [JPS_BASE_LIB] resource is ONLY required if a pre-release version is needed, which is (currently) not the case for this agent.

Simply execute the following command in the same folder as this `README` to build the required [Stack-Clients] resource and spin up the production version of the agent (from a bash terminal). The stack `<STACK NAME>` is the name of an already running stack.
bash


# Buildings the agent Docker image and pushing it
bash ./stack.sh build

# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK_NAME>


&nbsp;
# 2. Using the Agent
 

## Provided functionality

The agent will automatically register a task upon startup to assimilate the data in our target GPS folder into the KG. This background task can be triggered by an HTTP request after the agent starts up, accessible at http://localhost:5000/. 

<center>-------- Data Instantiation ------------</center>

- Post request to instantiate all electricity consumption/meter data to KG
> /fenlandtrajectoryagent/process_and_instantiate


&nbsp;
# Authors
Jiying Chen (jc2341@cam.ac.uk) April 2024
 
<!-- websites -->
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
[Dockerfile]: Dockerfile
[docker compose file]: docker-compose.yml
[docker-compose.test.yml]: tests\docker-compose.test.yml
[resources]: resources
[stack.sh]: stack.sh