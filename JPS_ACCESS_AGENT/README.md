# Access Agent

## Purpose

The purpose of the AccessAgent is to handle HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph. 
The agent will also perform requests to "get" and "insert" entire graphs. This agent extends the JPSAgent framework and can be called using methods 
in the AccessAgentCaller class in jps_base_lib.

## Spining up AccessAgent stack and setting up the AccessAgent in a dev environment

The AccessAgent stack (see docker-compose.yml) contains the Access Agent on port 48080 and an empty Blazegraph container on port 48081. The Blazegraph will host the store routing information. 
Images for the two containers are pulled from the Cambridge CARES container registry on GitHub and CMCL Docker image registry, respectively. Credentials are required to pull from the CMCL registry. 

From the command line, in same directory as this README, run:
```
docker-compose up -d --no-build
```
Next, you will need to populate the default "kb" namespace in Blazegraph with your routing information. This can simply be done using the Blazegraph user interface at `localhost:48081/blazegraph`.
Routing information consists of 5 triples e.g.
```
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>	"http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> "http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>.
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.
<http://www.theworldavatar.com/kb/ontokgrouter/ontokin>	<http://www.w3.org/2000/01/rdf-schema#label> "ontokin".
```
The queryStore(String targetResourceID, String sparqlQuery) and updateStore(String targetResourceID, String sparqlUpdate) methods from jps_base_lib can now be used with the local access agent by supplying a targetResourceID of the form `http://localhost:48080/<targetName>`, where <targetName> corresponds to the value of label in the routing information. For example, to use the local access agent to access ontokin based on the routing information shown above:
```
targetResourceID = http://localhost:48080/ontokin
```
Alternatively, in particular if developing a dockerized agent, the environment variable "ACCESSAGENT_HOST" should be set to "http://host.docker.internal:48080". In which case the targetResourceID can then just be passed as <targetName> i.e. "ontokin".
NOTE: JPS_BASE_LIB version "1.12.1-DEV-ACCESSAGENT-CONFIG-VARIABLES-SNAPSHOT" should be used.

## Building the AccessAgent

The AccessAgent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, you simply need to spin up a container/
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run
```
docker-compose up -d
```

The agent is reachable at "access-agent/access" on localhost port 48080.

## Integration Tests

The integration tests are designed to test the AccessAgentCaller class together with the AccessAgent, StoreRouter and the RemoteStoreClient and FileBasedStoreClient.

Requirements to run the AccessAgentRemoteStoreIntegrationTest:
1. 	Build and deploy the AccessAgent container locally with 'url.storerouter.endpoint=http://host.docker.internal:39889/blazegraph/namespace/kb/sparql' in main/resource/accessagent.properties
2.  Ensure the exposed port for the AccessAgent matches the 'accessagentHost' variable in AccessAgentRemoteStoreIntegrationTest (currently 48080)
2.	Comment out the "\@Disabled" annotation applied to the AccessAgentRemoteStoreIntegrationTest class
3.	Pull access to the CMCL Docker image registry (https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry) is required to deploy test containers

To run the test from the command line
```
mvn -Dtest=AccessAgentRemoteStoreIntegrationTest test
```