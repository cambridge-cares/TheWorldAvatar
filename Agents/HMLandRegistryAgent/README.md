# HM Land Registry Property Sales Instantiation Agent

The `Property Sales Instantiation` agent is an input agent which queries [HM Land Registry Open Data] and instantiates it according to the [OntoBuiltEnv] ontology in the [TheWorldAvatar] knowledge graph. More precisely, it queries the [Price Paid Linked Data] as well as the [UK House Price Index Linked Data] from the publicly available [HM Land Registry SPARQL endpoint].

After instantiating new property sales data, the agent also instantiates the relevant derivation mark-ups to allow for automatic assessment of the `Average Square Metre Price Per Postal Code` as well as the `Market Value Estimate per Property` (both implemented using the Derivation Framework).

The agent is designed to be deployed to a Docker stack spun up by the stack manager. 

&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image of the agent. 

&nbsp;
## 1.1 Prerequisites

Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):

### **1) The environment variables used by the agent container**

```bash
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data 
                      # NOTE: This must match the namespace where EPC data got instantiated
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver layer name, ALSO table name for actual building sales prices
GEOSERVER_WORKSPACE
BUILDINGS_TABLE       # PostGIS table containing all building footprints
                      # NOTE: This must match the table where EPC data got uploaded to
```

### **2) Derivation Framework**

The agent updates the timestamps of pure derivation inputs as well as instantiates and updates the required markups for both the `Average Square Metre Price per Postal Code` and the `Market Value Estimate per Property` derivations. Both of these derivations are initialised as synchronous derivations to create new info immediately. As the `Property Value Estimate` depends on the `Average Square Metre Price`, the latter one is marked up first to ensure availability of the required derivation input.

To ensure successful instantiation of the derivations, both derivation agents need to be successfully registered in the KG before requesting any update by the `Property Sales Instantiation` agent. The registered derivation agent IRIs need to match the value specified in [iris.py] (to be specified in their respective `docker-compose.yml` files).


### **3) Accessing Github's Container registry**

While building the Docker image of the agent, it also gets pushed to the [Container registry on Github]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Container registry on Github] simply run the following command to establish the connection and provide the access token when prompted:
```
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

### **4) Accessing CMCL docker registry**

The agent requires building the [Stack-Clients] resource from a Docker image published at the CMCL docker registry. In case you don't have credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page.

### **5) VS Code specifics**

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

This agent requires the [JPS_BASE_LIB] and the [Stack-Clients] to be wrapped by [py4jps]. Please note that compiling requires a [Java Development Kit version >=11]. However, *updating the resources is ONLY required if pre-release versions are needed.* Otherwise, the resources are automatically installed when building the Docker image.

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

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentication.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout main
$ git pull
```
Once the repository clone is obtained, please follow these instructions to [spin up the stack] on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

Before starting development of the dockerized agent remotely, all required VSCode extensions shall be installed on the remote machine (e.g. *augustocdias.tasks-shell-input* or the *Python extension*). To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

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

Agent start-up will automatically register a recurring task to assimilate latest sales transactions and UK house price index data for all instantiated properties every 4 weeks (i.e. new data is published monthly). Besides this recurring background task, additional HTTP requests can be sent to the agent.

The default SPARQL endpoint, i.e. namespace, used to assimilate the sales data shall be the same as used by the [EPC Agent] and is set by the `docker-compose.yml` script. In case another endpoint is required, the `docker-compose.yml` script needs to be updated accordingly. Furthermore, it needs to be noted that the `Property Sales Instantiation` agent requires building instances instantiated according to `OntoBuiltEnv` to function properly.

&nbsp;
## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5008/]. All requests are to be sent as POST requests and all available endpoints are listed below. Example requests are provided in the [resources] folder.

- POST request to update transaction record(s) for single property/list of properties:
  > `/landregistry/update`

- POST request to update transaction records for all instantiated properties, incl. property price indices for instantiated local authorities (i.e. most granular geography for which UK House Price Index is published):
  > `/landregistry/update_all`

Example requests are provided in the [resources] folder.

&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), March 2023


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Container registry on Github]: https://ghcr.io
[Java Development Kit version >=11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[OntoBuiltEnv]: http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[HM Land Registry Open Data]: https://landregistry.data.gov.uk/
[Price Paid Linked Data]: https://landregistry.data.gov.uk/app/root/doc/ppd
[UK House Price Index Linked Data]: https://landregistry.data.gov.uk/app/ukhpi/doc
[HM Land Registry SPARQL endpoint]: http://landregistry.data.gov.uk/landregistry/query

<!-- github -->
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[EPC Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/EnergyPerformanceCertificateAgent
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

<!-- files -->
[docker compose file]: ./docker-compose.yml
[resources]: ./resources
[iris.py]: ./agent/datamodel/iris.py
