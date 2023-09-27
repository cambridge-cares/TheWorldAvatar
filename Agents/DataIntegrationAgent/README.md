# Data Integration Agent


## Introduction
The `DataIntegration` agent is an input (and output) agent which has two main functions, one is to integrate name and address of building from OpenStreetMap to 3D buildings, the other is to extract footprint and calculate height of buildings.

The agent is implemented as Docker container to be deployed to a Docker stack spun up by the [Stack Manager] (https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). 

## Input
### Database config file
There are 6 parameters related to database connection. 
<ol>
  <li>URL to connect to the source postgreSQL database in Stack</li>
  <li>Username to connect to source postgreSQL database</li>
  <li>Password to connect to source postgreSQL database</li>
  <li>3D database name</li>
  <li>2D database name (For OpenStreetMap data in postgreSQL)</li>
  <li>2D database table (For OpenStreetMap data in postgreSQL)</li>
</ol>

### Input parameters
The agent accepts 2 input parameters in a HTTP request. 
1. function
    - spatiallink: spatial matching of OSM data and 3D buidling, then migrate name and address of building from OSM to 3D building
    - footprint: extract footprint of 3D building and store in postgresql
    - height: calculate height of 3D buidling and store in postgresql
2. thematic: to identify the building data has thematic surface or not (true/false)

Example input:
```
curl -X GET 'localhost:3838/data-integration/sql?function=footprint&thematic=false'
```



## Building the <i>Date Integration Agent</i>
1. The Data Integration Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

2. To set agent json file in stack-manager/inputs/config/services, and link it in the stack manager json file.

Example angent json file:
```
{
  "ServiceSpec": {
    "Name": "data-integration",
    "TaskTemplate": {
      "ContainerSpec": {
        "Image": "ghcr.io/cambridge-cares/data-integration:1.0.0",
        "Env": [
          "STACK_NAME=${STACK_NAME}",
          "DATABASE=postgres"
        ],
        "Configs": [
          {
            "ConfigName": "postgis"
          }
        ],
        "Secrets": [
          {
            "SecretName": "postgis_password"
          }
        ],
        "Mounts": [
          {
            "Type": "volume",
            "Source": "logs",
            "Target": "/root/.jps"
          }
        ]
      }
    }
  },
  "endpoints": {
    "data-integration": {
      "url": "http://localhost:8080/DataIntegrationAgent/",
      "externalPath": "/data-integration/"
    }
  }
}
```

Example stack manger json file:
```
{
	"services": {
		"includes": [
			"data-integration-debug"
		]
	}
}
```
3. To build the agent in Stack manager, open up the command prompt in the same directory as Stack manager, run
```
./stack.sh build
```
To start the agent in Stack manager, open up the command prompt in the same directory as Stack manager, run
```
./stack.sh start [STACK_NAME]
```


## Running the agent
To run the agent, a HTTP request must be sent to http://localhost:3838/DataIntegrationAgent/ with a correct JSON Object as described in Input. The command should be run in the same directory as this README
An example request is shown below.

```
curl -X GET 'localhost:3838/data-integration/sql?function=footprint&thematic=false'
```
