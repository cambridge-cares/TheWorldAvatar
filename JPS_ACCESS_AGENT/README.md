# Access Agent

## Purpose

The purpose of the AccessAgent is to handle HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph. 
The agent will also perform requests to "get" and "insert" entire graphs. This agent extends the JPSAgent framework and can be called using methods 
in the AccessAgentCaller class in jps_base_lib.

## The AccessAgent dev stack

The access-agent-dev-stack contains the Access Agent (on port 48888) and a Blazegraph (on port 48889). The purpose of the Blazegraph is to store the routing information used by the access agent in your dev environment. 
Routing information is stored in the default "kb" namespace and the access agent is configured to use this is as the STOREROUTER_ENDPOINT.

### Spining up the Access Agent dev stack

From the command line, in the access-agent-dev-stack directory, run:
```
docker-compose up -d --no-build
```
Images for the two containers are pulled from the Cambridge CARES container registry on GitHub and CMCL Docker image registry, respectively. 
Note: Credentials are required to pull from the CMCL registry. 

### Uploading routing information to the dev stack
To upload routing information. 
Populate the routing.json file in access-agent-dev-stack directory with the routing information you want to upload.
You need to provide a "label", "queryEndpoint" and "updateEndpoint" for each store/namespace. (The routing.json template contains two examples.)
Next, run the script uploadRouting.sh (in Linux or WSL).
```
bash ./uploadRouting.sh
```
Note: 
1. Routing triples can also be added 'manually' (e.g. through the user interface) to the "kb" namespace of the access-agent-dev-stack Blazegraph. 
2. The uploader will not overwrite information if a "label" already exists. You will need to do this 'manually'.

### Calling the Access Agent in your dev environment 

The access agent is available at http://localhost:48888 (or http://host.docker.internal:48888 if calling it from a docker container on windows).

The AccessAgent is usually called using the queryStore or updateStore found in the AccessAgentCaller and JPSAgent classes of JPS_BASE_LIB.
In order to call the access agent in your local dev environment the targetResourceID needs to be a url with the localhost and port number, e.g.
```
http://localhost:48888/label (or http://host.docker.internal:48888/label if calling the access agent from inside a docker container)
```
where label corresponds to the label uploaded to the router

## Building the Access Agent

The docker-compose and Dockerfile to build the Access Agent can be found in the docker-build directory.

### Version numbers
When building a new version of the access agent remember:
1. The AccessAgent version number on line 25 of the Dockerfile must match that in the AccessAgent pom.xml
2. The image version number in the docker-compose file should be updated
3. Also update the access agent image version in the dev stack to match the new version you are about to build and publish (../access-agent-dev-stack/docker-compose.yml)

### Building
To build the Access Agent image, in docker-build directory run:
```
docker-compose build
```

If building a new version of the image, the new image should be pushed to the GitHub container registry
```
docker push ghcr.io/cambridge-cares/access-agent:X.Y.Z
```
where X.Y.Z is the new version number.

