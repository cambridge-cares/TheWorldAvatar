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

### 3. Endpoint
When successfully built, the agent will be running at `port 3055`. The base url will be `http://localhost:3055/data-bridge-agent`.
There are currently two routes available

1. `<base>/status` route:
   - Returns the current status of the agent through an HTTP `GET` request.
2. `<base>/sparql` route:
    - Execute the agent's task through an HTTP `GET` request. This route will transfer data between the specified origin and destination endpoints
    - Before sending the request, please update the origin and destination SPARQL endpoint in the `<root>/config/endpoint.properties`.
    - A sample `GET` request is as follows:
```
curl -X GET localhost:3055/data-bridge-agent/sparql
```