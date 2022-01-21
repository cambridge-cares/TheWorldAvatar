# Description #

The `pyasyncagent` package provides a python wrapper for asynchronous agents as part of [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. It is a python equivalent of `uk.ac.cam.cares.jps.base.agent.AsynAgent.java` but based on [Flask](https://flask.palletsprojects.com/en/2.0.x/) application behind a [gunicorn](https://gunicorn.org/) server, inspired by the [Example: Python agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Deploy/examples/python_agent). `pyasyncagent` uses `py4jps>=1.0.15` to access asynchronous derivation operations provided in `uk.ac.cam.cares.jps.base.derivation.DerivationClient.java`. For technical details, below are a few useful links:
- [`py4jps`](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_wrapper) - python wrapper for jps-base-lib
- [`DerivationClient`](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation) - derivation framework
- [`AsynAgent.java`](https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/agent/AsynAgent.java) - asynchronous java agent that uses methods provided in `DerivationClient`
- [`DerivationAsynExample`](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/DerivationAsynExample) - example on chain of asynchronous derivations operated by asynchronous agents

# Installation #
For development and testing reasons, follow below instructions to get a copy of the project up and running on your local system.

## Virtual environment setup

It is highly recommended to install `pyasyncagent` packages in a [virtual environment](https://docs.python.org/3/tutorial/venv.html). The following steps can be taken to build a virtual environment:

`(Windows)`

```cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

`(Linux)`
```sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $
```

The above commands will create and activate the virtual environment `<venv_name>` in the current directory.

## Installation via pip

The following command can be used to install the `pyasyncagent` package and `agentlogging` package. This is a workaround as PyPI does NOT allow install_requires direct links, so we could NOT add package agentlogging from 'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils' as dependency, therefore, in order to make semi-automated release process working, we here introduce a workaround to install agentlogging to the virtual environment but NOT as dependency in the setup.py of `pyasyncagent`. A long term solution could be that we publish `agentlogging` in PyPI as well.

```sh
(<venv_name>) $ pip install pyasyncagent
(<venv_name>) $ pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils"
```

# How to use #

## Develop asynchronous derivation agent

When creating a new asynchronous derivation python agent, it's strongly advised to stick to the folder structure below:

*Recommended python asynchronous agent folder layout*

    .
    ├── ...                         # other project files (README, LICENSE, etc..)
    ├── src                         # project source files
    │   ├── agent                   # module that contains the agent logic
    │   │   ├── __init__.py
    │   │   └── your_agent.py       # your asynchronous agent
    │   ├── conf                    # module that specifies the configuration
    │   │   ├── __init__.py
    │   │   ├── agent_props.json    # json file that specifies the agent properties
    │   │   └── agent_props.py      # read the property json
    │   ├── data_model              # module that contains the dataclasses for concepts
    │   │   ├── __init__.py
    │   │   └── your_onto.py        # dataclasses for your ontology concepts 
    │   ├── kg_operations           # module that handles knowledge graph operations
    │   │   ├── __init__.py
    │   │   └── your_sparql.py      # sparql query and update strings for your agent
    │   ├── other_modules           # other necessary modules for your agent
    │   │   ├── __init__.py
    │   │   ├── module1.py
    │   │   └── module2.py
    │   └── entry_point.py          # agent entry point
    └── tests                       # tests files

*Example code*

`your_agent.py`

```python
from pyasyncagent import AsyncAgent
from kg_operations import YourSparqlClient
from data_model import YOUR_CONCEPT

class YourAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        # Provide your agent logic that converts the agent inputs to a list of new created instances
        # The agentInputs will be in the format of key-value pairs (python dict) with the concept as key and instance iri as value
        # For example: 
        # {
        #     "https://www.example.com/triplestore/repository/Ontology.owl#Concept_1": "https://www.example.com/triplestore/repository/Concept_1/Instance_1",
        #     "https://www.example.com/triplestore/repository/Ontology.owl#Concept_2": "https://www.example.com/triplestore/repository/Concept_2/Instance_2",
        #     "https://www.example.com/triplestore/repository/Ontology.owl#Concept_3":
        #         ["https://www.example.com/triplestore/repository/Concept_3/Instance_3_1", "https://www.example.com/triplestore/repository/Concept_3/Instance_3_2"],
        #     "https://www.example.com/triplestore/repository/Ontology.owl#Concept_4": "https://www.example.com/triplestore/repository/Concept_4/Instance_4"
        # }

        # You may want to create instance of YourSparqlClient for specific queries/updates you would like to perform
        # YourSparqlClient is defined in your_sparql.py that will be introduced later in this README.md file
        # This client can be initialised with the configuration you already initialised in YourAgent.__init__ method
        self.sparql_client = YourSparqlClient(
            self.kgUrl, self.kgUrl, self.kgUser, self.kgPassword
        )

        # Please note here we are accessing instance iri of class YOUR_CONCEPT within the provided agentInputs
        response = self.sparql_client.your_sparql_query(agentInputs[YOUR_CONCEPT])

        # You may want to log something during agent execution
        self.logger.info("YourAgent has done something.")

        # The returned value should be a list of new created instance, even if only one instance was created
        # For example: ['http://www.example.com/triplestore/repository/the_only_new_created_instance']
        return []
```

`agent_props.json`

```json
{
    "KNOWLEDGE_GRAPH": {
        "SPARQL_QUERY_ENDPOINT": "http://www.example.com/blazegraph/namespace/your_repo/sparql",
        "SPARQL_UPDATE_ENDPOINT": "http://www.example.com/blazegraph/namespace/your_repo/sparql",
        "KG_USERNAME": null,
        "KG_PASSWORD": null
    },
    "ONTOAGENT": {
        "ONTOAGENT_SERVICE": "http://www.example.com/resource/agents/Service__YourAgent#Service"
    },
    "DERIVATION_CLIENT": {
        "PERIODIC_TIMESCALE": 120,
        "DERIVATION_INSTANCE_BASE_URL": "http://www.example.com/triplestore/your_repo/"
    }
}
```
PLEASE NEVER COMMIT YOUR KG_USERNAME AND KG_PASSWORD TO GITHUB. A BETTER WAY OF ENCODING CREDENTIALS WILL BE PROVIDED IN THE NEXT RELEASE.

`agent_props.py`

For an example, please checkout: [`doeagent_properties.py`](https://github.com/cambridge-cares/TheWorldAvatar/blob/133-dev-design-of-experiment/Agents/DoEAgent/src/conf/doeagent_properties.py)

`your_onto.py`

```python
from pydantic.dataclasses import dataclass
from typing import Optional

# Ideally IRI of all concepts and relationships should already be provided in pyasyncagent.data_model.iris.py
# But just in case you are using some new concepts, you may define it here
YOUR_CONCEPT = 'https://www.example.com/triplestore/repository/Ontology.owl#YOUR_CONCEPT'

# You may also want to define dataclasses for some concepts you are working on
# Please note you need to define the concept before it gets referenced by others, as python is a scripting language
@dataclass
class YourOtherConcept():
    instance_iri: str

@dataclass
class YourConcept():
    instance_iri: str
    # Here you may want to declare an object relationship that points to YourOtherConcept
    # However, you may want to make it Optional with a default None
    example_object_relationship_to: Optional[YourOtherConcept] = None
```

`your_sparql.py`

```python
from pyasyncagent import PySparqlClient
from data_model import YourConcept

class YourSparqlClient(PySparqlClient):
    def your_sparql_query(self, your_instance_iri: str) -> YourConcept:
        # Construct your SPARQL query string
        query = """SELECT ?target WHERE {# your sparql query logic to get the information}"""

        # Perform SPARQL query with provided method performQuery in PySparqlClient class
        response = self.performQuery(query)

        # Instantiate TargetClass instances with query results
        new_target_instance = YourConcept(response[0]['target'])

        return new_target_instance

    def your_sparql_update(self, your_instance_iri: str):
        # Construct your SPARQL update string
        update = """INSERT DATA {# your sparql update logic to insert the triples}"""

        # Perform SPARQL update with provided method performUpdate in PySparqlClient class
        self.performUpdate(update)
```

`entry_point.py`

```python
from conf import AgentConfig
from agent import YourAgent

agent_config = AgentConfig(str(Path(__file__).absolute().parent) + '/conf/agent_props.json')

app = YourAgent(agent_config.ONTOAGENT_SERVICE, agent_config.PERIODIC_TIMESCALE, agent_config.DERIVATION_INSTANCE_BASE_URL, agent_config.SPARQL_QUERY_ENDPOINT)

if __name__ == '__main__':
    app.run()
```

You may refer to [DoEAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/133-dev-design-of-experiment/Agents/DoEAgent) for a concrete implementation of the above suggested folder structure based on `pyasyncagent`. The design of `pyasyncagent` is continually evolving, and as the project grows, we hope to make it more accessible to developers and users.

# New features and package release #

Developers who add new features to the python agent wrapper handle the distribution of package `pyasyncagent` on PyPI and Test-PyPI. If you want to add new features that suit your project and release the package independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your Test-PyPI and PyPI account and password
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine
- You have access to the docker.cmclinnovations.com registry on your local machine, for more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

Please create and checkout to a new branch from your feature branch once you are happy with the feature and above details are ready. The release process can then be started by using the commands below, depending on the operating system you're using. (REMEMBER TO CHANGE THE CORRECT VALUES IN THE COMMANDS BELOW!)

`(Windows)`

```cmd
$ cd \absolute_path_to\TheWorldAvatar\JPS_BASE_LIB\python_async_agent
$ release_pyasyncagent_to_pypi.sh -v x.x.x
```

`(Linux)`
```sh
$ cd /absolute_path_to/TheWorldAvatar/JPS_BASE_LIB/python_async_agent
$ ./release_pyasyncagent_to_pypi.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, change the version number in `JPS_BASE_LIB/python_async_agent/release_pyasyncagent_to_pypi.sh` to the one you used for the script release.
```sh
echo "./release_pyasyncagent_to_pypi.sh -v 0.0.3   - release version 0.0.3"
```

The changes mentioned above should then be committed with the changes performed automatically during the release process, specifically in python script `JPS_BASE_LIB/python_async_agent/pyasyncagent/__init__.py`
```
__version__ = "0.0.3"
```

and `JPS_BASE_LIB/python_async_agent/setup.py`
```
version='0.0.3',
```

Finally, merge the release branch back to the feature branch and make a Pull Request for the feature branch to be merged back into the `develop` branch.

# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
