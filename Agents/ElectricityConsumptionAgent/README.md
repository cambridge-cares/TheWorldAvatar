# Description

The `LSOAAnalyst agent` is dedicated to process data around the UK Lower-layer Super Output Area (LSOA), which is composited with feature of data download, instantiation, calculation and output.

Currently this agent is focusing on the data of electricity consumption, gas consumption, fuel poverty, climate (temperature) and geometric shape to perform the use case of analysing the deployment of heat pump. The data instatiated in the knowledge graph follows[Ontoclimate](http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl), [Ontofuelpoverty](http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl) and [Ontogasgrid](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/GasGrid) ontologies in the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar). 

The agent is implemented as Docker container to be deployed to a Docker stack spun up by the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). 

&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

## 1.1 Prerequisites

Retrieving [electricity consumption](https://www.gov.uk/government/statistics/lower-and-middle-super-output-areas-electricity-consumption), [gas consumption](https://www.gov.uk/government/statistics/lower-and-middle-super-output-areas-gas-consumption), [fuel poverty](https://www.gov.uk/government/statistics/sub-regional-fuel-poverty-data-2022) data from the GOV.UK do not need any preparation, upon runing the agent, those data will be downloaded automatically to the current folder. However, in order to download the hadUK climate grid files you need to registrate an account at [CEDA](https://services.ceda.ac.uk/cedasite/register/info/) platform. Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):


### **1) The environment variables used by the agent container**

```bash
# Agent configuration
YEAR                  # The year of which data you want to process
                      # e.g. '2020' (Set as default)
CEDA_USERNAME         # The username of the CEDA account
CEDA_PASSWORD         # The password of the CEDA account

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

### **3) VS Code specifics**

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

This agent requires [JPS_BASE_LIB] and [Stack-Clients] to be wrapped by [py4jps]. Therefore, after installation of all required packages (incl. `py4jps >= 1.0.26`), the `StackClients` resource needs to be added to allow for access through `py4jps`. All required steps are detailed in the [py4jps] documentation. However, the commands provided below shall suffice to compile the latest `StackClients` resource locally and install it inside the Docker container using the provided [Dockerfile]. Please note, that compiling requires a [Java Development Kit version >=11]. *Updating the [JPS_BASE_LIB] resource is ONLY required if a pre-release version is needed, which is (currently) not the case for this agent.*

Simply execute the following command in the same folder as this `README` to build the required [Stack-Clients] resource and spin up the *production version* of the agent (from a *bash* terminal). The stack `<STACK NAME>` is the name of an already running stack.
```bash
# Compiling latest StackClient py4jps resource
bash ./build_py4jps_stackclient_resource.sh

# Buildings the agent Docker image and pushing it
bash ./stack.sh build

# Deploying the agent (using pulled image)
bash ./stack.sh start <STACK_NAME>
```

&nbsp;
# 2. Using the Agent

## Default value in the code

> YEAR = '2020'
> QUERY_ENDPOINT= UPDATE_ENDPOINT = http://localhost:8080/blazegraph/namespace/ontogasgrid/sparql
> DB_URL = jdbc:postgresql:ts_example  \
> DB_USER = postgres  \
> DB_PASSWORD = postgres    

## Provided functionality

Agent start-up will automatically register a recurring task to assimilate latest data into the KG, i.e. electricity consumption, gas consumption, fuel poverty, climate (temperature) and geometric shape once per year. Besides those recurring background tasks, additional HTTP requests can be sent (but they might be delayed) to the agent. An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5000/]. All requests are to be sent as GET requests and all available endpoints are listed below:

<center>-----------Data Download ------------</center>

- GET request to download xslx file of UK subregional (LSOA) Electricity Consumption/meter data
> /api/electricityconsumptionagent/download/electricity
- GET request to download xslx file of UK subregional (LSOA) Gas Consumption/meter/nonmeter data
> api/electricityconsumptionagent/download/gas
- GET request to download xslx file of UK subregional (LSOA) fuel poverty data
> /api/electricityconsumptionagent/download/fuelpoverty
- GET request to download nc files of hadUK climate data in 1km grid
> /api/electricityconsumptionagent/download/temperature
<center>-------- Data Instantiation ------------</center>

- GET request to download and instantiate all Electricity Consumption/meter data to KG
> /api/electricityconsumptionagent/instantiate/electricity
- GET request to download and instantiate all UK subregional (LSOA) Gas Consumption/meter/nonmeter data to KG
> /api/electricityconsumptionagent/instantiate/gas
- GET request to download and instantiate all UK subregional (LSOA) fuel poverty data to KG
> /api/electricityconsumptionagent/instantiate/fuelpoverty
- GET request to download and instantiate all hadUK climate data in 1km grid to KG (GET request)
> /api/electricityconsumptionagent/instantiate/temperature
- GET request to download and instantiate all LSOA geometric shape to KG (GET request)
> /api/electricityconsumptionagent/instantiate/shape
- GET request to download and instantiate all data as mentioned above to KG
> /api/electricityconsumptionagent/instantiate/all



&nbsp;
# Authors
Jieyang Xu (jx309@cam.ac.uk), Feroz Farazi (msff2@cam.ac.uk) Dec 2022


(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com) and Tom Savage (trs53@cam.ac.uk))

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
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http
[resources]: resources
[stack.sh]: stack.sh
[tests]: tests