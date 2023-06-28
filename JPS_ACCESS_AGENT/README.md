# TripleStoreAccess Agent and RDBAccess Agent

## Purpose

The purpose of the TripleStoreAccessAgent and RDBAccessAgent is to provide access and/or routing to triple stores and relational databases respectively.

<b>Triple Store Access Agent:</B> The purpose of this agent is to handle HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph. 
The agent will also perform requests to "get" and "insert" entire graphs. This agent extends the JPSAgent framework and can be called using methods in the AccessAgentCaller class in jps_base_lib or by extending the JPSAgent class.

<b> RDB Access Agent: </b>The  purpose of this agent is to handle HTTP requests to obtain the url required to create a connection to a PostgreSQL database. This agent extends the JPSAgent framework 
and can be called using the getRDBUrl method in the RDBAccessAgentCaller class in jps_base_lib or by extending the JPSAgent class. 


<!------------------------------------------------------------->
<!-- ACCESS AGENT DEV STACK ----------------------------------->
<!------------------------------------------------------------->
## Deploying a local AccessAgent

The access-agent-dev-stack contains the <b>Triple Store Access Agent</b> and <b>RDBAccessAgent</b> (on port 48888) and a <b>Blazegraph</b> (on port 48889). 

The purpose of the Blazegraph is to store routing information used by the triple store access agent in your dev environment. 
Routing information is stored in the default "kb" namespace and the triple store access agent is configured to use this is as the STOREROUTER_ENDPOINT.

The uploading of routing information for the RDB Access Agent is described in the [Uploading Routing Information](#Uploading-routing-information) section below.

### Spinning up the Access Agent dev stack

From the command line, in the access-agent-dev-stack directory, run:
```
docker-compose up -d --no-build
```
Images for the two containers are pulled from the Cambridge CARES container registry on GitHub and CMCL Docker image registry. (Note: Credentials are required to pull from the CMCL registry. See https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry)

### Spinning up the Access Agent as part of a stack

Alternatively, if you want to spin up this agent as part of a stack, do not use `docker-compose`. Instead, do the following:
- In the `access-agent.json` file within the `access-agent-dev-stack` folder, adjust the image version if applicable, and replace the placeholder for the stack name in the endpoint environment variables with the name of your stack.
- Copy the `access-agent.json` file into the `inputs/config` folder of the stack manager.
- Start the stack manager as usual. This should start an access agent container as part of your stack.

### Uploading Routing Information

<b> Triple Store Access Agent: </b> 

In order to upload routing information into a store router blazegraph namespace, populate the `routing.json` file in access-agent-dev-stack directory with the routing information you want to upload.
You need to provide a `label`, `queryEndpoint` and `updateEndpoint` for each store/namespace. The `routing.json` file contains local, external, and stack examples. Note: the host `localhost` in the endpoint URL needs to be replaced by `host.docker.internal` (on Windows/Mac) or the docker network gateway IP (Windows/Mac/Linux).
Then, run the bash script `uploadRouting.sh`. NB If running the access agent within a stack, the port number in the access agent URL in the script will need to be adjusted.
```
bash ./uploadRouting.sh
```
Note: 
1. Routing triples can also be added manually (e.g. through the Blazegraph user interface 'http://localhost:48889/blazegraph') to the "kb" namespace of the access-agent-dev-stack Blazegraph. 
2. The uploader will not overwrite information if a "label" already exists. You will need to do this manually.

<b> RDB Access Agent: </b> 

1. In your local blazegraph, create a new namespace called 'ontordbrouter' to store the routing information. Alternatively, insert the routing information to the
ontordbrouter namespace at ``http://www.theworldavatar.com/blazegraph/``
2. In order to use your local ontordbrouter, locate the RDB_STOREROUTER_ENDPOINT in <i> access-agent-dev-stack/docker-compose.yml</i> and replace 
``http://www.theworldavatar.com/blazegraph/namespace/ontordbrouter/sparql`` with ``http://host.docker.internal:9999/blazegraph/namespace/ontokgrouter/sparql``. If using a different port for Blazegraph, replace 9999 with your port number.
3. For each PostgreSQL database to be accessed, 4 triples need to be added to the 'ontordbrouter' namespace. An example SPARQL update is shown, which inserts the triples required to access a 
database 'test'. Replace all occurrences of test with the name of the database and the endpoint with the location of your ontordbrouter namespace, and execute the SPARQL update.

    ```
    INSERT DATA {  
    <http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.theworldavatar.com/kg/ontordbrouter/hasUrl> "jdbc:postgresql://localhost:5432/test".
    <http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/kg/ontordbrouter/TargetRDBResource>.
    <http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.
    <http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/2000/01/rdf-schema#label> "test".
    }
    ```
   

### Calling the Agent in your dev environment 

The triple store access agent and RDB access agent are accessible at `localhost:48888`, or `host.docker.internal:48888` from inside a Docker container (on Windows/Mac), or, if running the agent within a stack, `localhost:3838` (by default) from outside the stack, `<STACK NAME>-access-agent:8080` from within.

The <b>Triple Store Access Agent</b> is usually called using the queryStore or updateStore found in the AccessAgentCaller and JPSAgent classes of JPS_BASE_LIB. Both methods take two arguments: the targetResourceID and the SPARQL query/update.

The <b>RDB Access Agent</b> is usually called using the getRDBUrl found in the RDBAccessAgentCaller and JPSAgent classes of JPS_BASE_LIB. The method takes one argument: the targetResourceID.

There are three ways to call your local TripleStoreAccess/RDBAccess agent:
1. Set url.accessagent.host in the jps.properties file in jps_base_lib.
2. Set the ACCESSAGENT_HOST environment variable. This is recommended if running your code from a docker container and can be done in the docker-compose file.
3. Alternatively, a full URL containing the correct host:port can be supplied as the targetResourceID e.g.
    ```
    http://localhost:48888/label or http://host.docker.internal:48888/label
    ```
    or, if running within a stack,
    ```
    http://<STACK NAME>-access-agent:8080/label,
    ```
    where the label corresponds to the label uploaded to the router.

<!------------------------------------------------------------->
<!-- BUILDING THE ACCESS AGENT -------------------------------->
<!------------------------------------------------------------->
## Building the Access Agent

The docker-compose and Dockerfile to build the Access Agent can be found in the docker-build directory.

### Version numbers
When building a new version of the access agent remember:
1. The AccessAgent version number on line 25 of the Dockerfile must match that in the AccessAgent pom.xml
2. The image version number in the docker-compose file should be updated
3. Also, please update the access agent image version in the dev stack to match the new version you are about to build and publish (../access-agent-dev-stack/docker-compose.yml)

### Building
To build the Access Agent image, in the docker-build directory run:
```
docker-compose build
```

If building a new version of the image, the new image should be pushed to the GitHub container registry
```
docker push ghcr.io/cambridge-cares/access-agent:X.Y.Z
```
where X.Y.Z is the new version number.

### Integration tests
Once built, the AccessAgentIntegrationTests and RDBAccessAgentIntegrationTests can be run on the new version of the AccessAgent 
by updating the ACCESS_AGENT_VERSION variable in the AccessAgentIntegrationTest class and the RDBAccessAgentIntegrationTest class
before running the tests.

The DeployedAccessAgentIntegrationTest can be run once the Access Agent has been deployed on The World Avatar server. 
This will test the agent in a production environment including connections to the ontokgrouter and ontordbrouter endpoints.