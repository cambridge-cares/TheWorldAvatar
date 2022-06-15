# Access Agent

## Purpose

The purpose of the AccessAgent is to handle HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph. 
The agent will also perform requests to "get" and "insert" entire graphs. This agent extends the JPSAgent framework and can be called using methods 
in the AccessAgentCaller class in jps_base_lib.

## Spining up AccessAgent stack and setting up the AccessAgent in a dev environment

The AccessAgent stack (see docker-compose.yml) contains the Access Agent on port 48080 and an empty Blazegraph container on port 48081. The Blazegraph container will host the store routing information. 
Images for the two containers are pulled from the Cambridge CARES container registry on GitHub and CMCL Docker image registry, respectively. Credentials are required to pull from the CMCL registry. 

From the command line, in same directory as this README, run:
```
docker-compose up -d --no-build
```
Next, you will need to populate the default "kb" namespace in Blazegraph with your routing information. This can be done using the Blazegraph user interface at `localhost:48081/blazegraph`.
Routing information consists of 5 triples e.g. for the "ontokin" triple store
```
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>	"http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> "http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>.
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/2000/01/rdf-schema#label> "ontokin".
```

## Calling the AccessAgent

Set the environment variable ACCESSAGENT_HOST to localhost:48080; or if in a docker container either 172.17.0.1:48080 or host.docker.internal:48080
The local AccessAgent can then be called using the queryStore and updateStore methods found in jps_base_lib by supplying a targetResourceID that corresponds to the label provided in the routing information (i.e. in the above example "ontokin"). 
NOTE: JPS_BASE_LIB version "1.12.1-DEV-ACCESSAGENT-CONFIG-VARIABLES-SNAPSHOT" or later must be used.

Alternatively, the desired host can be supplied via the targetResourceID in the form of a url. For example, targetResourceID = "http://localhost:48080/ontokin" will send the request to the local AccessAgent on port 48080. Whereas, targetResourceID = "http://www.theworldavatar.com:83/ontokin" will send the request to the default AccessAgent on Claudius.

If no ACCESSAGENT_HOST environment variable is set nor url supplied as the targetResourceID, the request will be sent to the default AccessAgent host at "http://www.theworldavatar.com:83".