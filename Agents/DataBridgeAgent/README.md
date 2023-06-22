# Data Bridge Agent
This agent serves as a bridge between the stack and external endpoints to enable seamless data interaction and transfer. It performs two primary functions:
1) Data Transfer: The agent facilitates the transfer of data between external SQL or SPARQL endpoints, and/or the knowledge graph deployed on the stack.
2) Time Series Data Instantiation: The agent also processes time series data for instantiation in the knowledge graph deployed on the stack.

## Instructions
### 1. Configurations
#### 1.1 Preparation
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide  your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

### 2. Building the Agent
The agent is designed for execution through a Docker container. Other deployment workflows are beyond the scope of this document.

#### 2.1 **TEST ENVIRONMENT**
- Deploy the agent to execute the unit and integration tests by running the following code in the CLI at the <root> directory. 
- Do note that the success of all tests must be verified through the test container's Docker logs. This container will only start executing the tests after the test database and agent containers are created.
```
docker compose -f "./docker/docker-compose.test.yml" up -d --build
```

#### 2.2 **DEVELOPMENT ENVIRONMENT**
- Deploy the agent for development and debugging by running the following code in the CLI at the directory. The debugger will be available at port 5005.
```
docker compose -f "./docker/docker-compose.debug.yml" up -d --build
```

#### 2.3 **PRODUCTION ENVIRONMENT**
- Deploy the agent and its dependencies by running the following code in the CLI at the `<root>` directory:
```
docker-compose up -d
```

#### 2.4 **STACK ENVIRONMENT**
- Follow the following instructions to deploy the agent within a stack.
  1) If the image is not available on the repository, build the image with the following tag at the `<root>` directory. Please change version number for your needs.
```
docker build -t data-bridge-agent:versionNo .
```
  2) Add the `<root>/docker/data-bridge-agent.json` to the [`stack-manager/inputs/config/services`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services) directory
     - Please ensure the version numbers are targeted at the right image. If you are building it, please update the version number accordingly.
  3) Modify the absolute path of the agent's `config` folder to your absolute file path
     - For Windows users using WSL on Docker, the file path should start with `/mnt/c/`, which is equivalent to `C://`
  4) Include this agent service into the stack configuration file at `stack-manager/inputs/config/<STACK-NAME>.json`
     - Read more in the [Stack Configuration](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) section
  5) Start the stack as per normal

If the agent is successfully started, the endpoint at `http://localhost:3838/data-bridge-agent/status` should return the following message.
```
{"Result":"Agent is ready to receive requests."}
```

### 3. Endpoint
When successfully built, the agent will be running at `port 3055`. The base url will be `http://localhost:3055/data-bridge-agent`.
There are currently four routes available:

1. `<base>/status` route:
   - Returns the current status of the agent through an HTTP `GET` request.
2. `<base>/sparql` route:
    - Execute the agent's task through an HTTP `POST` request. This route will transfer data between the specified source and target endpoints.
    - The request will require the following parameters:
      - `source`: The source SPARQL endpoint containing the triples to be transferred 
      - `target`: The target SPARQL endpoint intended to store the transferred triples
    - Sample SPARQL endpoints for Blazegraph are [listed here](#4-sample-blazegraph-endpoints)
    - A sample `POST` request using curl on a CLI:
```
curl -X POST --header "Content-Type: application/json" -d "{
    'source':'http://user:pass@ipaddress:port/blazegraph/namespace/kb/sparql',
    'target': 'http://user:pass@stackName-blazegraph:8080/blazegraph/namespace/kb/sparql'
}" localhost:3055/data-bridge-agent/sparql 
```
curl -X POST --header "Content-Type: application/json" -d "{'source':'http://10.25.188.130:9998/blazegraph/namespace/test/sparql', 'target': 'http://local-blazegraph:8080/blazegraph/namespace/test/sparql'}" localhost:3838/data-bridge-agent/sparql
3. `<base>/sql` route:
   - Execute the agent's task through an HTTP `GET` request. This route will transfer data between the specified source and target databases.
   - If there are existing tables in the target/destination database with the same name, those will be dropped and recreated. Please do take note of this side effect if you wish to retain old data.
   - Before sending the request, please read the instructions.
   - When transferring time series across non-stack database, please update the source and target jdbc url, user, and password in the `<root>/config/endpoint.properties`, and send the simple `GET` request.
     - Do note that in this instance, the transfer will not occur and will return a command instead. This command must be executed on a CLI that must have `psql` installed to transfer between two remote databases.
   - When transferring time series from or to the same stack's database, please send the `GET` request with the following parameters:
       - The `database` parameter refers to the specified database name within the same stack.
       - The `transfer` parameter indicates whether you wish to transfer time series into or outside the stack. `transfer=in` is for transferring time series from non-stack databases into the stack. `transfer=out` is for transferring time series from the stack to non-stack databases.
       - Please ensure that the non-stack database credentials have been updated in the `<root>/config/endpoint.properties` at the right position. The source database must be populated for `transfer=in`, whereas target database must be populated for `transfer=out`.
       - As the transfer may take some time, the agent may return a html response, but do kindly ignore it. The transfer is still occurring in the background.
```
# For any non-stack databases
curl -X GET localhost:3055/data-bridge-agent/sql
# For databases within the stack
curl -X GET 'localhost:3838/data-bridge-agent/sql?database=db&transfer=in'
```

4. `<base>/timeseries` route:
    - Execute the agent's task through an HTTP `POST` request using the [time series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries). This route will instantiate the time series inputs sent in the request into the stack's knowledge graph.
    - The request will require the following parameters:
        - `timeClass` : Refers to the time series classes as written in the [time series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries#instantiation-in-kg).
        - `timestamp` : A JSONArray containing the time stamp as strings in the format of `YYYY-MM-DD'T'HH:MM:SS`.
        - `values` : A JSONObject containing the time series values. A data IRI is inserted as the key and paired with their values as a JSONArray. For example: `{"dataIRI": [1, 2, 3]}`.
      - `namespace`: Specifies the SPARQL endpoint to store the instantiated time series data.  See [Sample Blazegraph endpoints](#4-sample-blazegraph-endpoints)
      - `database` (OPTIONAL) : Specifies the database name within the same stack. If not specified, the agent will instantiate the time series into the source JDBC url and credentials indicated in the `<root>/config/endpoint.properties` file.
    - A sample `POST` request using curl on a CLI:
```
curl -X POST --header "Content-Type: application/json" -d "{
    'timeClass':'INSTANTANEOUS',
    'timestamp': ['2022-11-09T03:05:18', '2022-11-19T03:05:18', '2022-11-29T03:05:18'],
    'values':{
        'electricity': [1,2,3],
        'energy': [4,5,6]
     },
        'namespace' ='http://user:pass@stackName-blazegraph:8080/blazegraph/namespace/kb/sparql'
        'database' = 'time',
     }" localhost:3838/data-bridge-agent/timeseries 
```

### 4. Sample Blazegraph endpoints
Below are the template various kind of Blazegraph-specific endpoints. Items enclosed in `<>` must be edited. Do note that `<url>` can either be in the `ipaddress:port` or `www.example.org` format

- Endpoint: `http://<url>/blazegraph/namespace/<namespace>/sparql`
- Authenticated endpoint: `http://<user>:<pass>@<url>/blazegraph/namespace/<namespace>/sparql`
- Stack endpoint: `https://<stackName>-blazegraph:8080/blazegraph/namespace/<namespace>/sparql`
- Authenticated stack endpoint: `https://<user>:<pass>@<stackName>-blazegraph:8080/blazegraph/namespace/<namespace>/sparql`
