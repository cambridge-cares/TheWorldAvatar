## Description
This `Resulted (Energy) Consumption Calculation` agent is designed to calculate the resulted electricity and gas consumption based on current energy consumptions, COP, with several assumptions and indecies, and instantiated in the [The World Avatar] KG according to the [OntoRegionalAnalysis] ontology. 

The agent is implemented as Docker container to be deployed to a Docker stack spun up by the [Stack Manager]. It is recommended to use `VS Code` to develop/deploy the agent. Hence, a few of the details below are VS Code specific.

This agent should be implemented with other agents using Derived Information Framework. This agent works with [LSOAInputAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/LSOAInputAgent), [CopCalculationAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CopCalculationAgent), [UtilityCostCalculationAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/UtilityCostCalculationAgent) and [InequalityIndexCalculationAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/InequalityIndexCalculationAgent), to support the analysis of the impact of heat pumps in the UK (for details refer to **[Preprint 323](https://como.ceb.cam.ac.uk/preprints/323/)** with **[Preprint 281](https://como.ceb.cam.ac.uk/preprints/281/)**). You may refer to the following graph to find what is the role of this agent in these calculations:

![Agent framework](https://i.imgur.com/vSBvBoJ.jpeg)

This agent uses Derived Infomation Framework, details provided below:


### Basic
The underlying equation of the calculations in this agent is:


$$
Change of gas consumption = uptake × gas consumption × propotion of heating
$$

$$
Change of electricityconsumption  = \frac{boiler efficiency × change of gas}{COP}
$$

$$
Resulted Gas consumption = Gas consumption - Change of gas consumption
$$

$$
Resulted Electricity consumption = Electricity consumption + Change of electricity consumption
$$

**Arguments**:

<u>Uptake</u>: a index which quantify the extent of the deployment of heat pump by measureing the gas being replaced by using heat pump comparing to conventional fossil fule heating system.
Defined as (change of gas / gas used for heating), or (change of gas / (total gas consumption * propotion of heating)).

<u>Gas consumption</u>: Value should be in unit of kWh.

<u>Electricity consumption</u>: Value should be in unit of kWh.

<u>Propotion of heating</u>: the propotion of gas which is used for heating(see equation above), if not provided, 0.9 will be used as default value.

<u>COP</u>: Coefficient of Performance. Details refer to COPCalculation agent.

<u>Boiler efficiency</u>: boiler efficiency (see equation above), if not provided, 0.8 will be used as default value.

<u>Electricity consumption profile</u>: A list containing 12 data represents the electricity consumption value from Jan to Dec.

<u>Gas consumption profile</u>: A list containing 12 data represents the gas consumption value from Jan to Dec.


**Example input:**
```python
query = {
'input':{'uptake': 0.5 ,
        'gas consumption': 5
        'electricity consumption': 5
        'propotion of heating': 0.9
        'cop': 2.5
        'boiler efficiency': 0.8
        'electricity consumption profile': [28.19,26.08,26.82,20.73,20.48,20.36,21.38,21.95,22.39,25.14,25.91,27.89]
        'gas consumption profile': [7.88,7.54,7.54,4.86,4.14,3.78,3.78,3.64,4.05,6.09,6.74,8.46]
        }
}
```

**Example output:**
```python

return = {'change of gas': 100 ,
        'change of electricity': 100
        }
```


### Required datasets
The required data comprimise the electricity consumption, gas consumption and COP data of the Lower Super Output Area (LSOA) in the UK . All data are updated annually. 
```bash
COP: # Available in the blazegraph, and can be calculated and instantiated by the [CopCalculationAgent]

Electricity consumption: # Available in the Ontop, a consumption csv file and config file, along with ontop.obda mapping file should be provided to enable the SPARQL query searching in the ontop. TODO: Details to be followed

Gas consumption: # Available in the Ontop, a consumption csv file and config file, along with ontop.obda mapping file should be provided to enable the SPARQL query searching in the ontop. TODO: Details to be followed
```

### Use the agent
The Resulted Consumption Calculation Agent is intended to use the `Asychronous mode` of the Derivation Framework to detect changes in instantiated [OntoRegionalAnalysis] properties (i.e. `COP`, `Electricity consumption`, `Gas Consumption`,`Boiler Efficiency`,`Proportion of Heating`,`Heat Pump Uptake`,`Energy Consumption Profile`.) and automatically updates associated `Resulted Energy Consumption`  instances in the KG. As the agent adopts the `pyderivationagent`, it also serves HTTP requests to handle synchronous derivations. However, it is (strongly) discouraged to invoke such HTTP request by ONESELF. 

After successful agent start-up, an instructional page shall become available at the root (i.e. `/`) of the port specified in the [docker compose file]. The exact address depends on where the agent container is deployed (i.e. localhost, remote VM, ...), but takes a form like `http://localhost:5200/`

### Agent Setup

Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file). For details on the [Derivation Agent configuration] please refer to the official documentation.

```bash
# Stack & Stack Clients configuration
STACK_NAME                    # Name of stack to which agent shall be deployed
DATABASE                      # PostGIS/PostgreSQL database name (default: `postgres`)
NAMESPACE                     # Blazegraph namespace (within Stack) to monitor
# Derivation Agent configuration
ONTOAGENT_SERVICE_IRI         # IRI of OntoAgent service
ONTOAGENT_OPERATION_HTTP_URL  # Port needs to match port specified in `docker-compose.yml`
DERIVATION_INSTANCE_BASE_URL  # Base IRI of all instanced generated by agent
REGISTER_AGENT                # Boolean flag whether to register agent in KG (`true` required to detect derivations)
# --- Index / Assumptions Space --- #
ELECTRICITY_CONSUMPTION_PROFILE # A list containing 12 data represents the consumption value from Jan to Dec
GAS_CONSUMPTION_PROFILE       # A list containing 12 data represents the consumption value from Jan to Dec
COP_VAR                       # Select the variable of COP to use: max / mean / min
UPTAKE                        # Assumed uptake of the heat pump in the hypothesis
PROPORTION_OF_HEATING         # Assumed proportion of the gas being used for heating
BOILER_EFFICIENCY             # Assumed the efficiency of the gas boiler
YEAR                          # The year of the index, will be used to instantiate the indecies in the KG
```

### Accessing Github's Container registry

While building the Docker image of the agent, it also gets pushed to the [Github container registry]. Access needs to be ensured beforehand via your github [personal access token], which must have a `scope` that [allows you to publish and install packages]. To log in to the [Github container registry] simply run the following command to establish the connection and provide the access token when prompted:
```
docker login ghcr.io -u <github_username>
<github_personal_access_token>
```

### VS Code specifics

In order to avoid potential launching issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

## Spinning up the stack

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

### Deploying the agent to the stack

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

## Asynchronous derivation operation
Once the Agent is deployed, it periodically (defined by `DERIVATION_PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE_IRI`) and acts based on the status associated with that derivation. Although the [Derivation Agent] suggests the use of `.env` files to specify environment variables for agent configurations, this approach does not work properly with Docker stacks, i.e. `docker stack deploy`. Hence, the agent configuration is moved into the [docker compose file] instead.

Details about how to use the agent please see the [home page] of this agent

## Upper level instances instatiation
If you started from an empty namespace, or have not instantiate upper level instances such as `country` or `assumption`, the result would not be able to be associated with them. 

Please check if you have created a namespace in the blazegraph, and entered the correct environmental variables in the [agent.env.example](./agent.env.example). 

Afterwards, run the [upper_level_ontology_update.py](./resultedconsumptioncalculationagent/upper_level_ontology_update.py), simply run this command in the powershell terminal:

```bash
py ./resultedconsumptioncalculationagent/upper_level_ontology_update.py
```
## Prior derivation markup

For the Agent to detect outdated information, a proper mark up of the relevant derivation inputs (i.e. *pure* inputs) is required. (Please note, that another pre-requisite for detecting derivation inputs is the registration of the agent in the KG, i.e. `REGISTER_AGENT=true` in the [docker compose file].) The [markup.py] example from the `pyderivationagent` package shall be used to mark up derivation inputs within the KG (for illustration purposes only), simply run this command in the powershell terminal:
```bash
py ./resultedconsumptioncalculationagent/markup.py
```

&nbsp;
# Authors #
Jieyang Xu (jx309@cam.ac.uk), May 2023
<!-- Links -->
[markup.py]:./resultedconsumptioncalculationagent/markup.py
[home page]:https://htmlpreview.github.io/?https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/ResultedConsumptionCalculationAgent/index.html
[CopCalculationAgent]:https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CopCalculationAgent
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Container registry on Github]: https://ghcr.io
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[Java Development Kit version >=11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[VS Code via SSH]: https://code.visualstudio.com/docs/remote/ssh
[visibility of the pushed docker image to public]: https://docs.github.com/en/packages/learn-github-packages/configuring-a-packages-access-control-and-visibility#configuring-visibility-of-container-images-for-an-organization

<!-- TWA github -->
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[Common stack scripts]: https://github.com/TheWorldAvatar/stack/tree/main/common-scripts
[Derivation Agent]: https://github.com/TheWorldAvatar/baselib/tree/main/python_derivation_agent
[Derivation Agent configuration]: https://github.com/TheWorldAvatar/baselib/tree/main/python_derivation_agent/pyderivationagent/conf/agent_conf.py
[EPC Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-EPCInstantiationAgent/Agents/EnergyPerformanceCertificateAgent
[JPS_BASE_LIB]: https://github.com/TheWorldAvatar/baselib/tree/main
[OntoRegionalAnalysis]: http://www.theworldavatar.com/ontology/ontoregionalanlysis/OntoRegionalAnalysis.owl
[HM Land Registry Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-PropertySalesInstantiationAgent/Agents/HMLandRegistryAgent
[spin up the stack]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/README.md#spinning-up-a-stack
[Stack Manager]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager
[Stack-Clients]: https://github.com/TheWorldAvatar/stack/tree/main/stack-clients
[The World Avatar]: https://github.com/cambridge-cares/TheWorldAvatar
[Average Square Metre Price Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AverageSquareMetrePriceAgent

<!-- data sources -->
[Energy Performance Certificate data]: https://epc.opendatacommunities.org/docs/api
[HM Land Registry Open Data]: https://landregistry.data.gov.uk/app/root/doc/ppd

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml