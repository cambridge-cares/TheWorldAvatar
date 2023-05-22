# Data Bridge Agent
This agent transfers the data stored on various SQL or SPARQL endpoints to the knowledge graph deployed on the stack.

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
- Do note that the success of all tests must be verified through the test container's Docker logs. This container will only start executing the tests after the test database containers and it are created.
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
There are currently three routes available:

1. `<base>/status` route:
   - Returns the current status of the agent through an HTTP `GET` request.
2. `<base>/sparql` route:
    - Execute the agent's task through an HTTP `GET` request. This route will transfer data between the specified source and target endpoints.
    - Before sending the request, please read the instructions.
    - When transferring triples across non-stack endpoint, please update the source and target SPARQL endpoint in the `<root>/config/endpoint.properties` and send the simple `GET` request.
      - For non-authenticated endpoints, please leave the corresponding `sparql.<src/target>.user` and `sparql.<src/target>.password` BLANK. 
      - If either endpoints are secured with username or password, please update their username and password accordingly for that specified endpoint.
    - When transferring triples from or to the same stack's endpoint, please send the `GET` request with the following parameters:
      - The `namespace` parameter refers to the stack SPARQL namespace, which is by default `kb`.
      - The `transfer` parameter indicates whether you wish to transfer triples into or outside the stack. `transfer=in` is for transferring triples from non-stack endpoints into the stack. `transfer=out` is for transferring triples from the stack to non-stack endpoints.
      - Please ensure that the non-stack endpoint have been updated in the `<root>/config/endpoint.properties` at the right position. The source endpoint must be populated for `transfer=in`, whereas target endpoint must be populated for `transfer=out`. 
```
# For any non-stack endpoints
curl -X GET localhost:3055/data-bridge-agent/sparql
# For namespaces within the stack
curl -X GET 'localhost:3838/data-bridge-agent/sparql?namespace=kb&transfer=in'
```

3. `<base>/sql` route:
   - Execute the agent's task through an HTTP `GET` request. This route will transfer data between the specified source and target databases.
   - If there are existing tables in the target/destination database with the same name, those will be dropped and recreated. Please do take note of this side effect if you wish to retain old data.
   - Before sending the request, please read the instructions.
   - When transferring time series across non-stack database, please update the source and target jdbc url, user, and password in the `<root>/config/endpoint.properties`, and send the simple `GET` request.
     - The request will return a list of commands to be executed on a CLI that must have `psql` installed to transfer between two remote databases.
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