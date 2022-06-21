# JPS Scenarios

## Purpose

JPS_SCENARIOS contains agents used for the parallel worlds framework. The purpose of the ScenarioAccessAgent is to handle HTTP requests to perform 
SPARQL query and update operations on RDF resources, as well as requests to "get" and "insert" entire graphs in the knowledge graph in a scenario 
(parallel world) context. The agent will also handle calls to other agents within a scenario context. This agent extends the JPSAgent framework and 
can be called using methods in the AccessAgentCaller class in jps_base_lib.
This project also contains a ScenarioManagementAgent and ScenarioModifier to create and manage scenarios. 

## Building JPS Scenarios

JPS Scenarios is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agents, you simply need to spin up a container/
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run
```
docker-compose up -d
```

The agents are reachable on localhost port 48081.
scenario access agent at "jps/scenario" 
scenario management agent at "jps/scenariomanagement" 
scenario modifier agent at "jps/scenariomod" 