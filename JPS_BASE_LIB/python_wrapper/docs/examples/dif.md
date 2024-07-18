The derived information framework (DIF) packaged in `twa` is a Python equivalent of `uk.ac.cam.cares.jps.base.agent.DerivationAgent.java` but based on [Flask](https://flask.palletsprojects.com/en/3.0.x/) application behind a [gunicorn](https://gunicorn.org/) server. This is inspired by the [Example: Python agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/examples/python_agent).

To read more about the Java-native implementation of `DerivationAgent` and derivation operations provided in `uk.ac.cam.cares.jps.base.derivation.DerivationClient.java`, please refer to below links:

- [`DerivationClient`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation) - derivation client Java classes that process the derivations
- [`DerivationAgent.java`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/DerivationAgent.java) - derivation agent Java class that uses methods provided in `DerivationClient`
- [`DerivationAsynExample`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAsynExample) - example on directed acyclic graph (DAG) of derivations operated by derivation agents in Java

To read the academic paper describing the DIF:

- Jiaru Bai, Kok Foong Lee, Markus Hofmeister, Sebastian Mosbach, Jethro Akroyd, and Markus Kraft. (2024). A derived information framework for a dynamic knowledge graph and its application to smart cities. Future Generation Computer Systems 152, 112–126. [doi:10.1016/j.future.2023.10.008](https://doi.org/10.1016/j.future.2023.10.008)

To read the academic papers using the DIF:

- Jiaru Bai, Sebastian Mosbach, Connor J. Taylor, Dogancan Karan, Kok Foong Lee, Simon D. Rihm, Jethro Akroyd, Alexei A. Lapkin, and Markus Kraft. (2024). A dynamic knowledge graph approach to distributed self-driving laboratories. Nature Communications 15, 462. [doi:10.1038/s41467-023-44599-9](https://doi.org/10.1038/s41467-023-44599-9)
- Wanni Xie, Feroz Farazi, John Atherton, Jiaru Bai, Sebastian Mosbach, Jethro Akroyd, and Markus Kraft. (2024). Dynamic knowledge graph approach for modelling the decarbonisation of power systems. Energy and AI 17, 100359. [doi:10.1016/j.egyai.2024.10035](https://doi.org/10.1016/j.egyai.2024.10035)
- Markus Hofmeister, Jiaru Bai, George Brownbridge, Sebastian Mosbach, Kok Foong Lee, Feroz Farazi, Michael Hillman, Mehal Agarwal, Srishti Ganguly, Jethro Akroyd, and Markus Kraft. (2024). Semantic agent framework for automated flood assessment using dynamic knowledge graphs. Data-Centric Engineering 5, 14. [doi:10.1017/dce.2024.11](https://doi.org/10.1017/dce.2024.11)
- Markus Hofmeister, George Brownbridge, Michael Hillman, Sebastian Mosbach, Jethro Akroyd, Kok Foong Lee, and Markus Kraft. (2024). Cross-domain flood risk assessment for smart cities using dynamic knowledge graphs. Sustainable Cities and Society 101, 105113. [doi:10.1016/j.scs.2023.105113](https://doi.org/10.1016/j.scs.2023.105113)
- Markus Hofmeister, Kok Foong Lee, Yi-Kai Tsai, Magnus Müller, Karthik Nagarajan, Sebastian Mosbach, Jethro Akroyd, and Markus Kraft. (2024). Dynamic control of district heating networks with integrated emission modelling: A dynamic knowledge graph approach. Energy and AI 17, 100376. [doi:10.1016/j.egyai.2024.100376](https://doi.org/10.1016/j.egyai.2024.100376)

A basic working example of using the DIF alone and in conjunction with the object graph mapper (OGM) is given below. Please refer to [Object Graph Mapper (OGM)](ogm.md) for examples on OGM itself.


## Develop derivation agent (server side)

An example agent that can be deployed using docker is provided here, it's strongly advised to stick to the folder structure below:

*Recommended Python agent folder layout*

    .
    ├── ...                         # other project files (README, LICENSE, etc..)
    ├── youragent                   # project source files for your agent
    │   ├── agent                   # module that contains the agent logic
    │   │   ├── your_agent.py       # your derivation agent
    │   │   └── __init__.py
    │   ├── conf                    # module that contains the agent config
    │   │   ├── your_conf.py        # specific configuration for your agent
    │   │   └── __init__.py
    │   ├── data_model              # module that contains the dataclasses for concepts
    │   │   ├── your_onto.py        # dataclasses (OGM) for your ontology concepts
    │   │   └── __init__.py
    │   ├── kg_operations           # module that handles knowledge graph operations
    │   │   ├── your_sparql.py      # sparql query and update strings for your agent
    │   │   └── __init__.py
    │   ├── other_modules           # other necessary modules for your agent
    │   │   ├── module1.py
    │   │   ├── module2.py
    │   │   └── __init__.py
    │   ├── entry_point.py          # agent entry point
    │   └── __init__.py
    ├── youragent.env.example       # example environment variables for configuration
    ├── docker-compose.yml          # docker compose file
    ├── Dockerfile                  # Dockerfile
    └── tests                       # tests files

Core script examples are shown below. Kindly see the inline documentation for clarifications.


### Agent class

`your_agent.py`

```python
from twa.agent import DerivationAgent
from twa.data_model.derivation import DerivationInputs
from twa.data_model.derivation import DerivationOutputs
from youragent.kg_operations import YourSparqlClient
from youragent.data_model import YourConcept
from youragent.data_model import AnotherConcept
from youragent.data_model import OutputConcept

# NOTE For any developer to extend the DerivationAgent class, four @abstractmethod MUST be implemented
# - agent_input_concepts = []
# - agent_output_concepts = []
# - validate_inputs(self, http_request) -> bool
# - process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs)
class YourAgent(DerivationAgent):
    ##########################################################
    ## I. __init__ for custom configuration (if applicable) ##
    ##########################################################
    def __init__(self,
        your_str_conf: str,
        your_int_conf: int,
        your_bool_conf: bool,
        **kwargs
    ):
        super().__init__(**kwargs) # pass all other parameters to DerivationAgent.__init__, will throw error if unexpected input received
        # Below you may want to assign your custom configuration
        # How to provide them when instantiating agent will be detailed in your_conf.py, entry_point.py, and youragent.env.example
        self.your_str_conf = your_str_conf
        self.your_int_conf = your_int_conf
        self.your_bool_conf = your_bool_conf

    ###############################
    ## II. Derivation agent part ##
    ###############################
    # Firstly, as the agent is designed to register itself in the knowledge graph when it is initialised
    # One need to define the agent inputs/outputs by providing the concept IRIs as return values
    # The registration is by default, which can be altered to False when instantiating agent
    # One way of doing it is setting flag REGISTER_AGENT=false in the env file

    # The agent inputs need to be provided in a list (even only one concept it involved)
    agent_input_concepts = [YourConcept, AnotherConcept]

    # The agent inputs need to be provided in a list (even only one concept it involved)
    agent_output_concepts = [OutputConcept]

    def validate_inputs(self, http_request) -> bool:
        # You may want to add some specific validation after the generic checks
        if super().validate_inputs(http_request):
            # do some specific checking
            pass

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Provide your agent logic that converts the agent inputs to triples of new created instances
        # The derivation_inputs will be in the format of key-value pairs with the concept as key and instance iri as value
        # For example:
        # {
        #     "https://example/kg/onto/YourConcept": ["https://example/kg/onto/YourConcept_1"],
        #     "https://example/kg/onto/AnotherConcept": ["https://example/kg/onto/AnotherConcept_1_1",
        #         "https://example/kg/onto/AnotherConcept_1_2"],
        # }

        # It is recommended to retrieve the agent inputs as OGM objects
        your_concept = derivation_inputs.get_inputs_ogm_assume_one(
            YourConcept, # the class you are interested in getting
            sparql_client, # sparql_client that is connected to the knowledge graph endpoint
            -1 # recursive_depth to control recursive queries
        )

        # The instance IRIs of the interested class (rdf:type) can be accessed via derivation_inputs.getIris(clz)
        # e.g. assume we will take the first iri that has rdf:type YOUR_CONCEPT
        instance_iri = derivation_inputs.getIris(YourConcept.get_rdf_type())[0]

        # You may want to create instance of YourSparqlClient for specific queries/updates not covered by OGM
        # YourSparqlClient should be defined in your_sparql.py that will be introduced later in this documentation page
        # This client can be initialised with the configuration you already initialised in YourAgent.__init__ method
        # A convenient method get_sparql_client is provided to get sparql_client if provided YourSparqlClient as arg
        sparql_client = self.get_sparql_client(YourSparqlClient)

        # Please note here we are using instance_iri of the YourConcept object within the provided derivation_inputs
        response = sparql_client.your_sparql_query(your_concept.instance_iri)

        # You may want to log something during agent execution
        self.logger.info("YourAgent has done something.")
        self.logger.debug("And here are some details...")

        # The new output can be created using OGM
        output_concept = OutputConcept(hasValue=5)
        # Which is essentially triples below:
        #   <https://example/kg/onto/OutputConcept_UUID> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/kg/onto/OutputConcept>.
        #   <https://example/kg/onto/OutputConcept_UUID> <https://example/kg/onto/hasValue> 5.

        # The new created instances should be added to the derivation_outputs
        # The easiest way to do it is by adding OGM objects
        derivation_outputs.add_outputs_ogm(output_concept)

    ################################################################
    ## III. Any other periodical job the necessary for your agent ##
    ################################################################
    # Additionally, if you would like to define your own periodical job to be executed, you may do it like blow:
    def your_periodical_job(self):
        # Here provide the job logic to be executed periodically
        pass

    @DerivationAgent.periodical_job # This decorator enables the _start_your_periodical_job() to be called when calling start_all_periodical_job()
    def _start_your_periodical_job(self):
        # You also need to provide a function so that your periodical job can be started on its own
        # self.scheduler is an object of APScheduler class
        self.scheduler.add_job(
            id='your_periodical_job', # the name for the periodical job
            func=self.your_periodical_job, # the function for the periodical job
            trigger='interval', # trigger type
            seconds=10 # specify this value to the time interval you prefer for the job execution
        )
    # The start of your periodical job can be done in two ways once instantiated YourAgent (assume an object named "your_agent"):
    # Option 1:
    # your_agent._start_your_periodical_job() # start your periodical job independently
    # Option 2:
    # your_agent.start_all_periodical_job() # start your periodical job together with all other periodical jobs (e.g. _start_monitoring_derivations)
    # An example is also provided in entry_point.py later in this documentation page
```


### Agent configuration

`your_conf.py`

```python
from twa.conf import AgentConfig
from twa.conf import config_generic

# Similar to AgentConfig, here you may provide the configurations specific to your agent
class YourConfig(AgentConfig):
    YOUR_STR_CONF: str
    YOUR_INT_CONF: int
    YOUR_BOOL_CONF: bool

def config_your_agent(env_file: str = None) -> YourConfig:
    """Return configurations from either environment variables or env_file."""
    # Remember to put YourConfig as the first input argument to config_generic
    return config_generic(YourConfig, env_file)
```


### OGM data models

`your_onto.py`

Please refer to [Object Graph Mapper (OGM)](ogm.md) for examples on OGM in more details.

```python
from __future__ import annotations # imported to enable pydantic postponed annotations
# The update_forward_refs() required in pydantic (1.x.x) to enable forward reference is no longer required for pydantic (2.x.x)
# Instead, `from __future__ import annotations` at the beginning of the script should already enable this
# For more details, please see https://pydantic-docs.helpmanual.io/usage/postponed_annotations/
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty, as_range

class YourOntology(BaseOntology):
    base_url = 'https://example/kg/'
    namespace = 'onto'
    owl_versionInfo = '0.0.1'
    rdfs_comment = 'ontology'

class HasValue(DatatypeProperty):
    is_defined_by_ontology = YourOntology
    range: as_range(int, 1, 1)

class YourConcept(BaseClass):
    is_defined_by_ontology = YourOntology
    hasValue: HasValue

class AnotherConcept(BaseClass):
    is_defined_by_ontology = YourOntology
    hasValue: HasValue

class OutputConcept(BaseClass):
    is_defined_by_ontology = YourOntology
    hasValue: HasValue
```

### Custom SPARQL client

`your_sparql.py`

Please refer to [Create custom sparql client](sparql.md/#create-custom-sparql-client) for more details.

```python
from twa.kg_operations import PySparqlClient

class YourSparqlClient(PySparqlClient):
    # PySparqlClient class provides a few utility functions that developer can call within their own functions:
    #  - checkInstanceClass(self, instance, instance_class)
    #  - getAmountOfTriples(self)
    #  - performQuery(self, query)
    #  - performUpdate(self, update)
    #  - uploadOntology(self, filePath)
    #  - uploadFile(self, local_file_path)
    #  - downloadFile(self, remote_file_path, downloaded_file_path)
    #  more to come...

    def your_sparql_query(self, your_instance_iri: str):
        pass
```


### Entry point for docker

`entry_point.py`

```python
from twa.conf import config_derivation_agent
from youragent.conf import config_your_agent
from youragent.agent import YourAgent

def create_app():
    # If you would like to deploy your agent within a docker container
    # (using docker-compose.yml and youragent.env which will be introduced later in this documentation page)
    # then you may use:
    agent_config = config_your_agent() # here we assume custom config are required, for normal config, you may use:
    # agent_config = config_derivation_agent()

    # Else, if you would like to create agent to run in your memory
    # then you may want to provide the path to youragent.env file as argument to function config_your_agent()
    # i.e.,
    # agent_config = config_your_agent("/path/to/youragent.env")
    # Again, for normal config, you may use:
    # agent_config = config_derivation_agent("/path/to/youragent.env")

    # Create agent instance
    agent = YourAgent(
        your_str_conf = agent_config.YOUR_STR_CONF, # remember to populate custom config if applicable
        your_int_conf = agent_config.YOUR_INT_CONF, # remember to populate custom config if applicable
        your_bool_conf = agent_config.YOUR_BOOL_CONF, # remember to populate custom config if applicable
        time_interval = agent_config.DERIVATION_PERIODIC_TIMESCALE,
        kg_url = agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url = agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user = agent_config.KG_USERNAME,
        kg_password = agent_config.KG_PASSWORD,
        fs_url = agent_config.FILE_SERVER_ENDPOINT,
        fs_user = agent_config.FILE_SERVER_USERNAME,
        fs_password = agent_config.FILE_SERVER_PASSWORD,
        derivation_instance_base_url = agent_config.DERIVATION_INSTANCE_BASE_URL,
        flask_config = FlaskConfig(),
        agent_endpoint_base_url = agent_config.ONTOAGENT_OPERATION_HTTP_BASE_URL,
        register_agent = agent_config.REGISTER_AGENT,
        logger_for_dev = True,
        # note that you can set the maximum number of threads to monitor async derivations at the same time
        max_thread_monitor_async_derivations = agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
        # note that you may choose NOT to supply below parameters if you DO NOT want email notifications
        email_recipient = agent_config.EMAIL_RECIPIENT,
        email_subject_prefix = agent_config.EMAIL_SUBJECT_PREFIX,
        email_username = agent_config.EMAIL_USERNAME,
        email_auth_json_path = agent_config.EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations = agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )

    # Start listening sync/monitoring async derivations
    # There are two ways of doing this, the first way it to start the monitoring process independently by:
    # Option 1:
    agent._start_monitoring_derivations()
    # Option 2:
    # Or, you can execute below line, which will start all periodical jobs that decorated with @DerivationAgent.periodical_job
    # where _start_monitoring_derivations() will be called as well
    # agent.start_all_periodical_job() # particularly useful when custom periodical job is defined

    # Expose flask app of agent to be picked by gunicorn
    app = agent.app

    return app
```


### .env file

`youragent.env.example`

```
DERIVATION_PERIODIC_TIMESCALE=60
DERIVATION_INSTANCE_BASE_URL=http://www.example.com/triplestore/repository/
SPARQL_QUERY_ENDPOINT=http://www.example.com/blazegraph/namespace/kb/sparql
SPARQL_UPDATE_ENDPOINT=http://www.example.com/blazegraph/namespace/kb/sparql
KG_USERNAME=
KG_PASSWORD=
FILE_SERVER_ENDPOINT=http://www.example.com/FileServer/
FILE_SERVER_USERNAME=
FILE_SERVER_PASSWORD=
ONTOAGENT_OPERATION_HTTP_BASE_URL=http://localhost:7000/YourAgent
REGISTER_AGENT=false
MAX_THREAD_MONITOR_ASYNC_DERIVATIONS=1
EMAIL_RECIPIENT=foo.1@bar.com;foo.2@bar.com
EMAIL_SUBJECT_PREFIX=YourAgent
EMAIL_USERNAME=your.gmail.address@gmail.com
EMAIL_START_END_ASYNC_DERIVATIONS=false

YOUR_STR_CONF=
YOUR_INT_CONF=
YOUR_BOOL_CONF=
```

You may want to commit this example file without credentials to git as a template for your agent configuration. At deployment, you can make a copy of this file, rename it to `youragent.env` and populate the credentials information. It is suggested to add `*.env` entry to your `.gitignore` of the agent folder, thus the renamed `youragent.env` (including credentials) will NOT be committed to git. For the usage of each default configuration, please refer to `twa.conf.AgentConfig` class.

> NOTE: you may want to provide `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT` as the internal port of the triple store (most likely blazegraph) docker container, e.g., `http://blazegraph:8080/blazegraph/namespace/kb/sparql`, if you would like to deploy your derivation agent and the triple store within the same docker stack (i.e. same docker-compose file, this is to be distinguished from the stack-client within The World Avatar, which is to be integrated into `twa` in the future iterations). In the endpoint, `blazegraph:8080` depends on your specification in the `docker-compose.yml` which will be introduced in [Docker compose file](#docker-compose-file).
>
> An alternative to this is to add `extra_hosts: - host.docker.internal:host-gateway` to the `your_agent` service in `docker-compose.yml` (as shown in [Docker compose file](#docker-compose-file)) - then you can access the blazegraph via `http://host.docker.internal:27149/blazegraph/namespace/kb/sparql` (`host.docker.internal:27149` depends on your specification in the `docker-compose.yml`).
>
> Please also note that the host and port of `ONTOAGENT_OPERATION_HTTP_BASE_URL` (i.e., `localhost:7000` in `http://localhost:7000/YourAgent`) should match the value provided in the `docker-compose.yml` to ensure it is resolvable for handling synchronous derivations once registered in the knowledge graph.
>
> At deployment, configurations in this file will be picked up by `config_derivation_agent()` when instantiating the agent in `create_app()` of `entry_point.py` (see [Entry point for docker](#entry-point-for-docker)).


### Docker compose file

`docker-compose.yml`

```yml
version: '3.8'

services:
  # Your derivation agent
  your_agent:
    image: your_agent:1.0.0
    container_name: your_agent
    environment:
      LOG4J_FORMAT_MSG_NO_LOOKUPS: "true"
      # Add email auth json path that to be read by the yagmail service
      EMAIL_AUTH_JSON_PATH: /run/secrets/email_auth
    build:
      context: .
      dockerfile: ./Dockerfile
    ports:
      - 7000:5000
    # Note that "host.docker.internal" is only a placeholder string, you can replace it with anything, e.g. "localhost" (HOWEVER, NOTE THAT "localhost" IS NO LONGER WORKING AS OF py4jps 1.0.23 (predecessor of twa), WHEREAS ANY OTHER PLACEHOLDER STRING STILL WORKS, AS DETAILED IN ISSUE https://github.com/cambridge-cares/TheWorldAvatar/issues/347)
    # But please be aware that this can be unstable on some versions docker-desktop as noticed by other developers:
    # https://github.com/docker/for-win/issues/8861
    extra_hosts:
      - host.docker.internal:host-gateway
    env_file:
      - ./youragent.env

  # Blazegraph
  blazegraph:
    image: ghcr.io/cambridge-cares/blazegraph:1.1.0
    container_name: "blazegraph_test"
    ports:
      - 27149:8080
    environment:
      # Use BLAZEGRAPH_USER and BLAZEGRAPH_PASSWORD_FILE if you would like to add authentication
      # Otherwise, you may wish to comment them out
      BLAZEGRAPH_USER: bg_user
      BLAZEGRAPH_PASSWORD_FILE: /run/secrets/blazegraph_password
    # Add a secret to set the password for BASIC authentication
    secrets:
      - blazegraph_password

# Secrets used to set runtime passwords
secrets:
  blazegraph_password:
    file: tests/dummy_services_secrets/blazegraph_passwd.txt
  email_auth: # You may want to add below file name to your .gitignore
    file: tests/dummy_services_secrets/email_auth.json

```

The design of derivation agent in `twa` is continually evolving, and as the project grows, we hope to make it more accessible to developers and users.


### Set up email notification for exceptions
The `DerivationAgent` class provides the feature to send email notifications to list of recipients specified by the developer. As the agent uses [yagmail](https://github.com/kootenpv/yagmail) package, a gmail account is required. The feature relies on [OAuth2](https://oauth.net/2/) for authorisation. A step-by-step instruction can be find [here](https://github.com/kootenpv/yagmail/issues/143#issuecomment-1161223461).


## Create a-/sync derivations (client side)

Methods are provided in the `PyDerivationClient` to create derivation instances that are responded by agents in different timescales: synchronous (quick calculations) and asynchronous (lengthy computations).

### Instantiate derivation client

The first step is to instantiate a derivation client:

```python
from twa.data_model.iris import TWA_BASE_URL
from twa.kg_operations.derivation_client import PyDerivationClient

# assume the below SPARQL endpoint is available
sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'

derivation_client = PyDerivationClient(
    derivation_instance_base_url=TWA_BASE_URL, # you may choose to provide your own base url
    query_endpoint=sparql_endpoint,
    update_endpoint=sparql_endpoint,
    # you may also provide credentials if the above endpoint is password-protected
    # kg_user=<username>,
    # kg_password=<password>,
)
```


### Synchronous derivations

Given below IRIs, two types of derivations can be marked up for synchronous agent responses.

```python
outputs = ['https://example/kg/output_1', 'https://example/kg/output_2>']
agent_iri = 'https://example/kg/MyAgent'
inputs = ['https://example/kg/input_1', 'https://example/kg/input_2']
```

1. Derivation without time series
    ```python
    derivation_iri = derivation_client.createDerivation(
        entities=outputs,
        agentIRI=agent_iri,
        inputs=inputs,
    )
    ```

2. Derivation with time series
    ```python
    derivation_iri = derivation_client.createDerivationWithTimeSeries(
        entities=outputs,
        agentIRI=agent_iri,
        inputs=inputs,
    )
    ```

Derivations can also be created for generating new information, i.e. the outputs of this derivation are to be generated (either with or without time series). When the HTTP endpoint of the agent to handle the synchronous derivation is unknown to the calling entity, one can use the below method:

```python
from twa.data_model.iris import ONTODERIVATION_DERIVATION
from twa.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES

# create a derivation instance
derivation = derivation_client.createSyncDerivationForNewInfo(
    agentIRI=agent_iri,
    inputsIRI=inputs,
    derivationType=ONTODERIVATION_DERIVATION,
    # below line can be used instead to create derivation with time series
    # derivationType=ONTODERIVATION_DERIVATIONWITHTIMESERIES
)
```

When calling this method, the framework will query the agent HTTP endpoint and fire an HTTP request to request for computing new information with the provided inputs.

> *NONE* of the `inputsIRI` should be derived information of asynchronous derivations.

Should the developer already know the URL of the HTTP endpoint, one can provide such HTTP endpoint to the method by calling function `createSyncDerivationForNewInfoWithHttpUrl`:

```python
derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
    agentIRI=agent_iri,
    agentURL=agent_http_endpoint,
    inputsIRI=inputs,
    derivationType=ONTODERIVATION_DERIVATION,
    # below line can be used instead to create derivation with time series
    # derivationType=ONTODERIVATION_DERIVATIONWITHTIMESERIES
)
```

> *NOTE that developer MUST make sure the provided `agentIRI` and `agentURL` matches with each other.*
> If the developer has access to the agent object, then one can use: `agent.agentIRI` and `agent.syncDerivationEndpoint`

The returned type of the above methods is `twa.data_model.derivation.Derivation`, developer can get the IRI of the created derivation and list of IRIs of the generated outputs via:

```python
# the generated derivation IRI
derivation_iri = derivation.getIri()

# the IRIs of generated new information with a rdf:type of interest 'http://this_is_a_specific_rdfType'
output_iris = derivation.getBelongsToIris('http://this_is_a_specific_rdfType')
```

This initialisation thus supports creating a graph of synchronous derivations on-the-fly without creating placeholder instances in the knowledge graph when only pure inputs exist:

```python
# create a downstream derivation that takes the outputs of the derivation instance created just now as inputs
downstream_derivation = derivation_client.createSyncDerivationForNewInfo(
    agentIRI=another_agent_iri,
    inputsIRI=output_iris,
    derivationType=ONTODERIVATION_DERIVATION
)

# again we retrieve its outputs with the interested rdf:type
# 'http://specific_rdfType_of_downstream_derivation_outputs'
outputs_of_downstream_derivation = downstream_derivation.getBelongsToIris(
    'http://specific_rdfType_of_downstream_derivation_outputs'
)

# we can then combine these inputs to form yet another downstream derivation
yet_another_downstream_derivation = derivation_client.createSyncDerivationForNewInfo(
    agentIRI=yet_another_agent_iri,
    inputsIRI=output_iris + outputs_of_downstream_derivation,
    derivationType=ONTODERIVATION_DERIVATION
)
```


### Asynchronous derivations

Currently, asynchronous derivations only work with non time series data. To create an asynchronous derivation for new information:

```python
# assume that we are creating a downstream derivation from a normal instance and an existing derivation
inputs_and_derivations = ['https://example/kg/input_1', 'https://example/kg/existing_derivation']

derivation_iri = createAsyncDerivationForNewInfo(
    agentIRI=agent_iri,
    inputsAndDerivations=inputs_and_derivations
)
```

This initialisation supports creating new derivation that depends on upstream derivations which themselves can be created for generating new information, i.e. no outputs yet:

```turtle
<derivation_iri> OntoDerivation:isDerivedFrom <https://example/kg/existing_derivation>
```

Once the upstream derivation finishes generating new information, e.g. generated `<newEntity1>` and `<newEntity2>`, the derivation framework will handle the reconnection of new information to the derivation structure in the knowledge graph:

```turtle
<newEntity1> OntoDerivation:belongsTo <https://example/kg/existing_derivation>
<newEntity2> OntoDerivation:belongsTo <https://example/kg/existing_derivation>
<derivation_iri> OntoDerivation:isDerivedFrom <newEntity1>
<derivation_iri> OntoDerivation:isDerivedFrom <newEntity2>
```

This feature allows users to create a directed acyclic graph (DAG) of the derivation to form a workflow that is to be executed.


### Validate created derivation DAGs
Once the derivation instances are initialised using the above methods, one could use the provided method to check that the connection is valid:

```python
# returns a bool
derivation_client.validateDerivations()
```

This method goes through all the inputs of the provided derivation, and all the subsequent inputs if the inputs are derivations. This makes sure that there are no circular dependencies, and each instance has a valid timestamp.



## Request derivation update (client side)
### Pure synchronous response

Four methods exist to request update of the derivation instances if all the derivations you want to update are synchronous derivations, i.e. instances of `OntoDerivation:Derivation` or `OntoDerivation:DerivationWithTimeSeries`:

```python
# to update a single sync derivation
derivation_client.updatePureSyncDerivation(derivation_iri)

# to update a list of sync derivations
derivation_client.updatePureSyncDerivations(list_of_derivation_iris)

# to update a list of sync derivations in parallel
derivation_client.updatePureSyncDerivationsInParallel(list_of_derivation_iris)

# to update all sync derivations in a knowledge graph
derivation_client.updateAllSyncDerivations()
```


### Asynchronous operation

To request update of the derivation instance if the derivation you want to update is an asynchronous derivation, i.e. instance of `OntoDerivation:DerivationAsyn`.

```python
derivation_client.updateMixedAsyncDerivation(derivation_iri)
```


### Mixed type - async derivations depend on sync derivations

To update a directed acyclic graph (DAG) that consists of async derivations depending on sync derivations:

```python
derivation_client.unifiedUpdateDerivation(derivation_iri)
```

In this mode, it will mark the derivation and all its dependencies as update `OntoDerivation:Requested` if it is determined as outdated (including sync derivations). The agent is expected to monitor the derivation that `OntoDerivation:isDerivedUsing` itself periodically and check if any requested asynchronous derivations. For those requested, the agent checks the status of its upstream derivations and will wait if any of its immediate upstream asynchronous derivations are still outdated - the agent only acts when it determined all its immediate upstream asynchronous derivations are up-to-date. It will first request update of all its upstream sync derivations (if any), and then set up job for requested. For more insights, you may refer to below for a demo.


## Multi-agent system - combine everything above

A minimal working example of a multi-agent systems combining everything above is provided in the format of dockerised integration tests. following the same context as [`DerivationAsynExample`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAsynExample). Interested developer may refer to the README of the Java example for more context, or `TheWorldAvatar/JPS_BASE_LIB/python_wrapper/tests` for more technical details.

To check the example in more details, one may execute below commands for each set of dockerised integration test:

- Agents instantiated and run in memory, operating on a blazegraph docker container with dynamic port decided on deployment

    `(Linux)`
    ```sh
    cd /<absolute_path_to>/TheWorldAvatar/JPS_BASE_LIB/python_wrapper
    pytest -s tests/test_derivation_agent.py
    ```

- All agents and blazegraph deployed within the same docker stack and they communicate via internal port address

    `(Linux)`
    ```sh
    cd /<absolute_path_to>/TheWorldAvatar/JPS_BASE_LIB/python_wrapper
    pytest -s tests/test_docker_integration.py
    ```

Ideally, we would like to provide this set of dockerised integration test to demo how one may develop integration test for their own derivation agents. Any ideas/discussions/issues/PRs on how to make this more standardised and accessible to developers are more than welcome.
