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
- If successful, the container will be created but shut down immediately. Otherwise, test errors will be printed to the CLI.
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
  1) Build the image with the following tag at the `<root>` directory:
```
docker build -t data-bridge-agent:versionNo .
```
  2) Add the `<root>/docker/data-bridge-agent.json` to the [`stack-manager/inputs/config/services`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services) directory
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
    - Before sending the request, please update the source SPARQL endpoint in the `<root>/config/endpoint.properties`.
    - If transferring to any other endpoint, please update the target SPARQL endpoint and send the simple `GET` request.
    - If transferring within the same stack's endpoint, please leave the target SPARQL endpoint empty, and send the `GET` request with the following parameter.
      - The `namespace` parameter refers to the target stack SPARQL namespace, which is by default `kb`.
```
# For any destination
curl -X GET localhost:3055/data-bridge-agent/sparql
# For namespaces within the stack
curl -X GET localhost:3055/data-bridge-agent/sparql?namespace=kb
```

3. `<base>/sql` route:
   - Execute the agent's task through an HTTP `GET` request. This route will transfer data between the specified source and target databases.
   - Before sending the request, please update the source and target database urls, user, and password, in the `<root>/config/endpoint.properties`.
   - A sample `GET` request is as follows:
```
curl -X GET localhost:3055/data-bridge-agent/sql
```