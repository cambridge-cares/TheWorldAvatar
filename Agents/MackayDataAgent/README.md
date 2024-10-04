# Mackay Data Agent
The ` Mackay Data Agent` manages the update and reading of the dynamic data from TWA knowledge graph which is required as the input data to Singapore Mackay Calculator Model. Human users or other agents can make Http requests to this agent to initiate an update of these data or read the data.

As a proof of concept, we have mapped several input entries of the Mackay Calculator Model to real-time [OntoTimeSeries] (TS) data in the TWA Knowledge Graph. These TS data are created under the [Derived Information Framework]'s (DIF) to ensure proper data provenance and manageable data update. Each required datapoint is the result of a chain of derivation actions. For example, Mackay Calculator requires the yearly dwelling unit of Singapore from 2015 to 2050 as input. This TS data is a Forecast derivation of the real data from 2015 to 2020 (via [Forecasting Agent]), which in turn is instantiated from the raw data downloaded from an external web API. The derivation from the API meta information to a downloaded TS data is managed by the [API Agent]. DIF provides both sync and async library methods to update these data along the derivation chain, allowing easy management of the updating action despite the complex data dependency of a model that may require thousands of input data, such as the Mackay Calculator.

Note that this agent serves as the example of a data client that manages the update and reading of multiple DIF-data in KG, each resulting from a chain of derivations. However, for the current implementation of this agent, the first derivation has to be an API-agent derivation, which then undergone none or another derivation which is either a Forecast or an algebra calculation. To make this agent more generic is plausible and we leave it for future work.






# 1. Setup

## 1.1 Agent Configuration
### 1.1.1 Agent Base Configuration
Agent base configuration can be modified in [base.cfg].
```bash
[rdb_access] # RDB Access
db_url = 
db_user = 
db_pw = 

[kg_access] # Triple Store Access
endpoint = 

[api_agent] # API Agent
agent_url =
agent_iri =

[forecast_agent] # Forecast Agent
agent_url = 
agent_iri = 

[output] # BASE URL of triples to create by this agent
base_url = 
```
### 1.1.2 Data Point Configuration

#### 1.1.2.1 Timeseries Data
Mackay Data Agent manages the update and reading of multiple data points, each the result of a chain of derivations. Configuration files one per data point are put under [data config folder]. Some examples can be found in `./confs_files/data` folder. 

Before deployment, remove any of the unused data configuration files from `./confs_files/data` folder.

 New data points can easily be added by creating new files in the folder following the syntax below.
```bash
[output]
steps = forecast # Derivation Step after API download, in [forecast|calculation]
target_iri = https://test/Number_dwellingunit # IRI of target quantity
api_iri = https://test/TSMap_dwellingunit # IRI of the API map instance
name = dwelling_unit

# Forecast parameters
[forecast]
predictEnd = 2050
frequency = 1
unitFrequency = year
```

#### 1.1.2.2 Meta Data
The Mackay Data Agent is able to handle meta data queries to the knowledge graph as well. Some examples can be found in the `./confs_files/data` folder. 
- `variable_name` - name of the variable
- `url` - query endpoint
- `user` - username to access query endpoint if any exist
- `password` - password to access query endpoint if any exist
- `query_string` - SPARQL query strings

## 1.2 Required Derivation Markup
`Mackay Data Agent` is a client of the [API Agent]. If any api-meta-data IRI declared in the data config files is not registered to API agent and in turn never instantiated when the Mackay Data Agent starts, `Mackay Data Agent` will register them to the [API agent] (and subsequently the API data will be instantiated). This however is on the condition that the API meta-data triples are already properly instantiated in the KG.  Refer to [API Agent] README 1.3 on how to declare API meta-data in KG.

`Mackay Data Agent` is also a client of the [Forecasting Agent]. However, there is no need for the user to manually insert the forecast meta-data triples, as the agent will automatically instantiate them according to the data config files.

Note that this design choice is made as API-downloaded data are supposed to be used by multiple agents; therefore it is conceptually not sound to keep a copy of API meta-information in this one client agent.


## 1.3. Using the Agent
The agent image can be built via:
```
docker compose -f "docker-compose.yml" up -d --build
```
Note that the agent needs 4 other components to work:
1. Blazegraph.
2. PostgresSQL.
3. [Forecasting Agent]
4. [API Agent]

# 2. Agent Design

# 2.1 Mackay Calculator Agents
The Singapore MacKay Carbon Calculator provides a model of the Singapore energy system that allows you to explore pathways to decarbonisation. The scenario simulation is based on an underling Mackay Calculator Model. We implement it in the TWA agent framework with two agents.
1) [Mackay Calculator Agent]: The agent that exposes a web-interface and level-controlling API of the Singapore Mackay Calculator Model. This Agent needs to be deployed on a Windows OS due to its dependency on Excel.
2) Mackay Data Agent (this agent): The agent that manages the update of the input data to the Mackay Calculator Model as derived Timeseries (TS) datapoints in TWA. The required input data are derived as Timeseries under DIF with a chain of [Forecasting Agent] and [API Agent].

Note that it is possible to deploy Calculator Agent without the Data Agent, however in that case the input data of the Calculator model will not be dynamically updated from the TWA KG.



# 2.2 Data Agent Operation
The full time-sequence diagram of Mackay Data Agent actions are illustrated as the below diagram. 
1) If any API Data requires by the Data Agent is not yet registered, the Data agent requests the registration of this API Data via creating a derivation from the meta-API-mapping triples via the API Agent.
2) Any required API data are managed and updated by the API Agent periodically.
3) Client may request an update of all data managed by the Data Agent via http request. Upon request, each datapoint is updated via the DIF derivation update method, which checks the attached timestamps along the derivation chain and updates accordingly.
4) Client may request to read the datapoints from the current KG.

# 2.3 Ontology
We map three Mackay inputs data to quantity instances in TWA KG. A proposed version of design can be found in [tbox_dev]. Note that this is not the finalized version.













<!-- Links -->
<!-- websites -->
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Github container registry]: https://ghcr.io
[personal access token]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
[Derived Information Framework]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation
[derivation agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent
[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries
[OntoDerivation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoderivation
[API Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/APIAgent
[Forecasting Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent
<!-- files -->
[HTTP forecast error request]: ./resources/HTTP_evaluate_errors.http
[model mapping]: ./forecastingagent/fcmodels/model_mapping.py
[docker compose file]: ./docker-compose.yml
[stack manager input config file]: ./stack-manager-input-config/forecasting-agent.json
[stack-manager-input-config]: ./stack-manager-input-config
[test_plots]: tests/test_plots/
[Web of Things (WoT) Hypermedia Controls Ontology]:https://www.w3.org/2019/wot/hypermedia
[RDF Mapping Language (RML)]:https://rml.io/specs/rml/
[base.cfg]:./confs_files/base.cfg
[data config folder]: ./confs_files
[Mackay Calculator Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MackayCalculatorAgent
[tbox_dev]: ./tbox_dev/quantity_triples