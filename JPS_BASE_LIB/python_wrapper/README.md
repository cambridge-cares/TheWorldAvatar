# TheWorldAvatar (twa)

`twa` is a Python wrapper for [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. It expands on the TWA's Java functions with Python-native capabilities.


## What is `twa`

The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access Java objects in a Java Virtual Machine. It has a precedent python package, `py4jps`, which is now deprecated.

To get started, see the [Quick start](#quick-start) below or follow our [tutorial](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper/docs/tutorial/).


## Installation

To install `twa`, use the following command:
```pip install twa```

You also need to install a Java Runtime Environment version 11:
- **[Recommended]** If you are using Linux (or Windows Subsystem for Linux): ```apt install openjdk-11-jdk-headless```
- If you are using Windows machine: please follow the tutorial [here](https://learn.microsoft.com/en-us/java/openjdk/install)


## Quick start
```python
from __future__ import annotations

###############################################
# Spin up a docker container for triple store #
###############################################
import docker
# Connect to Docker using the default socket or the configuration in your environment:
client = docker.from_env()

# Run Blazegraph container
# It returns a Container object that we will need later for stopping it
blazegraph = client.containers.run(
    'ghcr.io/cambridge-cares/blazegraph:1.1.0',
    ports={'8080/tcp': 9999}, # this binds the internal port 8080/tcp to the external port 9998
    detach=True # this runs the container in the background
)


#############################
# Instantiate sparql client #
#############################
from twa.kg_operations import PySparqlClient

# Define the SPARQL endpoint URL for the Blazegraph instance
sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'

# Create a SPARQL client to interact with the Blazegraph endpoint
sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)


################################################
# Upload an ontology from an internet location #
################################################
# Example: Upload the PROV ontology from the web
prov_ttl = 'https://www.w3.org/ns/prov.ttl'
from rdflib import Graph

# Parse the ontology and upload it to the triple store
sparql_client.upload_graph(Graph().parse(prov_ttl))


########################
# Perform some queries #
########################
# Example query: Retrieve subclasses of prov:Agent
results = sparql_client.perform_query(
    """
    prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    prefix prov: <http://www.w3.org/ns/prov#>
    select *
    where {?agent rdfs:subClassOf prov:Agent}
    """
)
print(results)
# Expected output:
# > [{'agent': 'http://www.w3.org/ns/prov#Organization'},
# > {'agent': 'http://www.w3.org/ns/prov#Person'},
# > {'agent': 'http://www.w3.org/ns/prov#SoftwareAgent'}]


#########################
# Create a new ontology #
#########################
from twa.data_model.base_ontology import BaseOntology, BaseClass, TransitiveProperty, ObjectProperty, DatatypeProperty
from twa.data_model.iris import TWA_BASE_URL
from typing import ClassVar, Optional

# Define a minimal agent ontology
class MinimalAgentOntology(BaseOntology):
    base_url: ClassVar[str] = TWA_BASE_URL
    namespace: ClassVar[str] = 'mao'
    owl_versionInfo: ClassVar[str] = '0.0.1'
    rdfs_comment: ClassVar[str] = 'A minimal agent ontology'

# Define classes and properties for the ontology
class Agent(BaseClass):
    rdfs_isDefinedBy = MinimalAgentOntology
    name: Name[str]
    hasGoal: HasGoal[Goal]
    # Like native Pydantic, you can define optional fields (properties)
    actedOnBehalfOf: Optional[ActedOnBehalfOf[Agent]] = None

class Goal(BaseClass):
    rdfs_isDefinedBy = MinimalAgentOntology
    priority: Priority[str]

Name = DatatypeProperty.create_from_base('Name', MinimalAgentOntology, 1, 1)
"""
This is equivalent to:

class Name(DatatypeProperty):
    rdfs_isDefinedBy = MinimalAgentOntology
    owl_minQualifiedCardinality = 1
    owl_maxQualifiedCardinality = 1
"""
Priority = DatatypeProperty.create_from_base('Priority', MinimalAgentOntology, 1, 1)

HasGoal = ObjectProperty.create_from_base('HasGoal', MinimalAgentOntology)

# Another way of defining properties
class ActedOnBehalfOf(TransitiveProperty):
    rdfs_isDefinedBy = MinimalAgentOntology


#######################################
# Export the TBox to the triple store #
#######################################
# Export the ontology definition (TBox) to the triple store
MinimalAgentOntology.export_to_triple_store(sparql_client)


####################################
# Instantiate some objects as ABox #
####################################
# Create instances (ABox) of the ontology classes
machine_goal = Goal(
    rdfs_comment='continued survival',
    priority='High'
)
machine = Agent(
    name='machine',
    hasGoal=machine_goal
)
smith_goal = Goal(
    rdfs_comment='keep the system in order',
    priority='High'
)
agent_smith = Agent(
    name='smith',
    actedOnBehalfOf=machine,
    hasGoal=smith_goal
)

# Push the instances to the knowledge graph
agent_smith.push_to_kg(sparql_client, -1)


########################
# Perform some queries #
########################
# Retrieve all instances of the Agent class from the knowledge graph
agents = Agent.pull_all_instances_from_kg(sparql_client, -1)

# Once the objects are pulled, the developer can access information in a Python-native format
# Example: Print out the goals of each agent
for agent in agents:
    print(f'agent {agent.name} has goal: {agent.hasGoal}')
# Expected output:
# > agent {'smith'} has goal: {Goal(rdfs_comment='keep the system in order', ...)}
# > agent {'machine'} has goal: {Goal(rdfs_comment='continued survival', ...)}
```


## Documentation
The documentation for `twa` can be found [here](https://cambridge-cares.github.io/TheWorldAvatar/).


## Issues? Feature requests?
Submit an [issue](https://github.com/cambridge-cares/TheWorldAvatar/issues) with a label `python-wrapper`.
