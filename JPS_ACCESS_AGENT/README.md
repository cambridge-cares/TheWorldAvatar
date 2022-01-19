# Access Agent

## Purpose

The purpose of the AccessAgent is to handle HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph. 
The agent will also perform requests to "get" and "insert" entire graphs. This agent extends the JPSAgent framework and can be called using methods 
in the AccessAgentCaller class in jps_base_lib.

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
1. 	AccessAgent running on Docker
2.	Comment out the "\@Disabled" annotation applied to the AccessAgentRemoteStoreIntegrationTest class
3.	Pull access to the CMCL Docker image registry (https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry)
4.	Internet connection to the OntoKGRouter triple store (currently at `http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql`)
5. 	OntoKGROuter is expected to contain routing to endpoint `http://172.17.0.1:39888/blazegraph/namespace/kb/sparql` for the resource "teststorelocal"

To run the test from the command line
```
mvn -Dtest=AccessAgentRemoteStoreIntegrationTest test
```