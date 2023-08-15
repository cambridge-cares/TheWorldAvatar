# Dashboard Agent
The Dashboard Agent is designed to set up and populate dashboards within a stack. These dashboards will require both spatial topological and time series data to be available within the stack.
Namely, it will require the concept of buildings, facilities, rooms, elements and connected sensors/devices from at minimal the [OntoBIM](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobim) and [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) ontologies.

## Instructions
Before you can use the Dashboard Agent, there are some requirements you need to meet. Follow the steps below to ensure you have everything you need to successfully run the agent.
### 1. Agent Deployment
The agent is designed for execution through a Docker container within a stack. It cannot run as a standalone container, and other deployment workflows are beyond the scope of this document. 
Follow the steps below to build and deploy the agent for either the test or production environment.

#### 1.1 Preparation
##### Maven Repository credentials
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

##### Stack containers
This agent requires the following tools, which **MUST** run on the same stack. Please read more from the [stack manager page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for setting these containers up.

(1) [Grafana](https://grafana.com/docs/grafana/latest/) dashboard
- Required for this agent to configure and set up dashboards
- Image version: **grafana-oss:9.5.2** 
- Mandatory configuration url: `http://<STACK_NAME>-grafana:3000`

(2) PostGIS database
- Contains the time series data

(3) SPARQL endpoint
- Contains triples linking time series to facilities and/or assets
- Mandatory structure:
  - A name must be appended to all buildings, facilities, assets, sensors, and measures/dataIRIs through the `Instance rdfs:label "name"^^xsd:string` triple.
  - All sensor measures are attached according to the [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) ontology.

#### 1.2 Docker Deployment
- Build this agent's image by issuing `docker compose build` within this folder. Do not start the container.
- Copy the `dashboard-agent.json` file from the `stack-manager-input-config` folder into the `inputs/config/services` folder of the stack manager, adjusting the absolute path of the bind mount as required. 
Please review the [different routes](#2-agent-route) to understand the purpose of these bind mounts. See [sample bind mounts](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#bind-mounts) for the configuration syntax.
- Start the stack manager as usual.

### 2. Agent Route
The agent currently offers two API routes:
#### 2.1 GET ROUTE: `~url~/dashboard-agent/status` 
This route requires a GET request without any parameters, to retrieve the agent's current status. A sample request for curl syntax (in one line) is as follows:
```
curl localhost:3838/dashboard-agent/status 
```
If the agent ran successfully, a JSON Object would be returned as follows:
```
{"Result":["Agent is ready to receive requests."]}
```

#### 2.2 GET ROUTE: `~url~/dashboard-agent/setup`
The agent will require a GET request without any parameters and a configuration file called `credentials.properties` placed at the `<root>/config/` directory of your bind mount. This will contain the
configuration for:
- `dashboard.user`: The username for the dashboard container running within the stack. Default for Grafana is admin.
- `dashboard.pass`: The password for the dashboard container running within the stack. Default for Grafana is admin.

A sample request for curl syntax (in one line) is as follows:
```
curl localhost:3838/dashboard-agent/setup 
```
If the agent ran successfully, a JSON Object would be returned as follows.
```
{"Result":["Dashboard has been successfully set up!"], "Runtime": time}
```