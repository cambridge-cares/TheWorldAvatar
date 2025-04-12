# Access Agent
### Purpose 
The purpose of this agent is to provide access and/or routing to two kinds of resources in the knowledge graph - triple stores and relational databases. The Access Agent hides the storage implementation layer, so that the user or calling agent does not need to know the location or software e.g. Blazegraph, RDF4J etc. On one hand, it handles HTTP requests to perform SPARQL query and update operations on RDF resources in the knowledge graph as well as to "get" and "insert" entire graphs. On another hand, it handles HTTP requests to access the relational database url using an identifier. As an extension of The World Avatar's agent framework, this agent's methods can be called **EITHER** using the `AccessAgentCaller`/`RDBAccessAgentCaller` class in the `jps_base_lib` package **OR** by extending the JPSAgent class.

## 1. Build Instructions

The Access agent is available as a Docker image in this [package](https://github.com/cambridge-cares/TheWorldAvatar/pkgs/container/access-agent). Users need not build the image unless they are developing a newer version. The latest version can be pulled from this [repository](https://github.com/cambridge-cares/TheWorldAvatar/pkgs/container/access-agent).

Keep reading if you need to build the image for any reason or skip to the [deployment section](#2-deployment-instructions).

#### Build Step 1

This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central). You'll need to provide your credentials (GitHub username/personal access token) in a single-word text files located like this:

```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

#### Build Step 2

The docker related files for building the Access Agent can be found in the `./docker-build` directory. If changes have been made to the agent, update the version number or append a `-SNAPSHOT` qualifier by following these steps:

1. The AccessAgent version number on line 25 of the `./docker-build/Dockerfile` must match that in the AccessAgent `./access-agent-code/pom.xml`
2. Update the image version number in the `./docker-build/docker-compose.yml` file accordingly
3. Update the image version in the dev stack to match this updated version you are about to build and publish -- `./access-agent-dev-stack/access-agent.json` and `./access-agent-dev-stack/docker-compose.yml`

Once the versions have been updated, run the following at the `<root>/docker-build` directory to build the Docker image:

```
docker compose build
```

If building a new version of the image, the new image should be pushed to the GitHub container registry by running:

```
docker push ghcr.io/cambridge-cares/access-agent:X.Y.Z
```

where X.Y.Z is the new version number. Please also ensure that you are logged in to the docker registry. Follow [step 1 of this](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager#spinning-up-a-stack) for clarity.

#### Integration tests

Once built, the `AccessAgentIntegrationTests` and `RDBAccessAgentIntegrationTests` can be run on the new version of the AccessAgent
by updating the `ACCESS_AGENT_VERSION` variable in the `AccessAgentIntegrationTest` class and the `RDBAccessAgentIntegrationTest` class
before running the tests.

The `DeployedAccessAgentIntegrationTest` can be run once the Access Agent has been deployed on The World Avatar server.
This will test the agent in a production environment including connections to the `storerouter` and `storerouterrdb` SPARQL endpoints.

## 2. Deployment Instructions

The Access Agent can be deployed in a standalone Docker container or as part of The World Avatar [stack](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager).

### 2.1 Standalone Container

Developers can deploy the access agent with a Blazegraph container when developing the access agent by running the following in the command line from the `./access-agent-dev-stack` directory:

```
docker-compose up -d --no-build
```

- Note that the images for the two containers are pulled from the Cambridge CARES container registry on GitHub and require credentials. See https://github.com/cambridge-cares/TheWorldAvatar/wiki/Using-Docker-images

When successful, two containers will be running on `port 48888` (Access Agent) and `http://localhost:48889/blazegraph/` (Blazegraph). The purpose of the Blazegraph is to store routing information used by the access agent. This agent container has been configured to store SPARQL routing information in the default `kb` namespace using the `STOREROUTER_ENDPOINT` environment variable. RDB routing information is configured to be stored in the `ontordbrouter` namespace using the `RDB_STOREROUTER_ENDPOINT` environment variable. Further details for these configuration is described in the [Uploading Routing Information](#31-uploading-routing-information) section.

### 2.2 Stack Deployment

In order to deploy this agent as part of a stack, execute the following steps:

- In the `./access-agent-dev-stack/access-agent.json` file , adjust the image version (if applicable). `REPLACE` the `<STACK NAME>` placeholder in the environment variables with the name of your stack. Doing so will ensure the routing information is uploaded to the `storerouter` and `storerouterrdb` namespaces for the SPARQL and RDB routing information respectively.
- Copy the `access-agent.json` file into the `inputs/config` folder of the stack manager.
- Start the stack manager as usual. This should start an access agent container as part of your stack.

## 3. Usage Instructions

### 3.1 Uploading Routing Information

Before the agent can be called in any environment, the routing information must be uploaded to the `storerouter` and `storerouterrdb` namespaces for the SPARQL and RDB routing information respectively. This can be set using the corresponding `STOREROUTER_ENDPOINT` and `RDB_STOREROUTER_ENDPOINT` environment variables to change the namespace names. See the `./access-agent-dev-stack/docker-compose.yml` or `./access-agent-dev-stack/access-agent.json` for examples. Please note that a `label` must be included in order to ensure that the agent can target the right resource.

> Triple Store Routing Information

The Access Agent requires routing information for the target resource to be contained in the specified `storerouter` namespace. Routing information for the triple store namespaces can be uploaded manually or programmatically. Regardless of the method, the routing information consists of 5 triples containing the SPARQL query and update endpoints as well as a label identifying the target resource. An example for the **ontokin** namespace is as follows:

```
// Note that `<ontokin>` is replaceable with the label you require
// SPARQL endpoints
<http://www.theworldavatar.com/kb/storerouter/<ontokin>>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>	"http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".
<http://www.theworldavatar.com/kb/storerouter/<ontokin>>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> "http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql".

// Classes
<http://www.theworldavatar.com/kb/storerouter/<ontokin>>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>.
<http://www.theworldavatar.com/kb/storerouter/<ontokin>>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.

// Label used as an identifier for accessing the right namespace
<http://www.theworldavatar.com/kb/storerouter/<ontokin>>	<http://www.w3.org/2000/01/rdf-schema#label> "<ontokin>".
```

These routing triples can be added manually (e.g. through the Blazegraph user interface 'http://localhost:48889/blazegraph') following the SPARQL specifications. Alternatively, programmatically, the `./access-agent-dev-stack/routing.json` file must be populated with the following fields for each namespace. The file contains examples for local host, external host, and The World Avatar stack. Do note that the host `localhost` in the endpoint URL needs to be replaced by `host.docker.internal` (on Windows/Mac) or the docker network gateway IP (Windows/Mac/Linux).

- `label`
- `queryEndpoint`
- `updateEndpoint`

Once the file is populated, run the bash script `./access-agent-dev-stack/uploadRouting.sh` using the `bash ./uploadRouting.sh` command in the `./access-agent-dev-stack` directory. Adjust the `accessagentURL` field if running within a stack. Note that if a "label" already exists, the uploader will not overwrite any information. Instead, please manually modify the triples on the namespace.

> RDB Routing Information

1.  In the knowledge graph, create a new namespace based on the specified `RDB_STOREROUTER_ENDPOINT` environment variable (default is `ontordbrouter`) to store the routing information. If you wish to use another namespace, please adjust your Docker environment variables accordingly to function in the stack or standalone containers.
2.  For each PostgreSQL database to be accessed, 4 triples need to be added to the namespace in (1). Example triples for accessing a database `test` is shown below. Note to replace all occurrences of `test` with your required database name to ensure the label for the target resource works.

```
// Database url
<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.theworldavatar.com/kg/ontordbrouter/hasUrl> "jdbc:postgresql://localhost:5432/test".

// Classes
<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/kg/ontordbrouter/TargetRDBResource>.
<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.

// Label used as an identifier for accessing the database
<http://host.docker.internal:9999/blazegraph/ontordbrouter/test> <http://www.w3.org/2000/01/rdf-schema#label> "test".
```

### 3.2 Calling the Access Agent

Developers should call the access agent in two ways:

1. If you are developing a Java agent within the TWA agent framework (by extending the class `uk.ac.cam.cares.jps.base.agent.JPSAgent`), users can directly call methods inherited from `JPSAgent` to access these resources
2. In the case you are not extending `JPSAgent`, users can import the class `uk.ac.cam.cares.jps.base.query.AccessAgentCaller` or `uk.ac.cam.cares.jps.base.query.RDBAccessAgentCaller` to call the methods accessing the resources. The `AccessAgentCaller` contains the `queryStore` and `updateStore` methods, whereas the `RDBAccessAgentCaller` contains the `getRDBUrl` method.

Explanation of the relevant methods are as follows:

1. `queryStore(String targetResourceID, String query)`: Retrieves data from the knowledge graph using SPARQL queries
2. `updateStore(String targetResourceID, String updateQuery)`: Modifies the data within the knowledge graph using SPARQL queries
3. `getRDBUrl(String targetResourceID)`: Retrieves the specified relational database url from the knowledge graph

As shown, all methods require a targetResourceID in the url format of `domain/label`, where label is the required resource identifier (matching the label in the routing information). The `domain` component of the url can follow one of the following forms:

- `localhost:48888`: External access outside a Docker container
- `host.docker.internal:48888`: Internal access within a Docker container (on Windows/Mac)
- `localhost:PORT-NO`: External access from a running stack; `PORT-NO` is `3838` by default
- `<STACK NAME>-access-agent:8080`: Internal access within the stack

If running within the JPS agent framework, developers can set a global environment variable storing the core domain url for The World Avatar's knowledge graph. This has been set as the `url.accessagent.host` field in the `JPS_BASE_LIB/src/main/resources/jps.properties` file. Users can call this variable to retrieve the domain using `uk.ac.cam.cares.jps.base.config.KeyValueMap.getInstance().get(uk.ac.cam.cares.jps.base.config.IKeys.URL_ACCESSAGENT_HOST)`. However, we do not recommend doing so. Instead, it is recommended that developers should modify their agents to retrieve the domain from user inputs to allow the agents to be generalisable and reusable in other applications.
