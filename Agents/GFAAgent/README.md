# GFAAgent
## 1. Description
The GFAAgent aims to calculate gross floor area of buildings. It includes three parts: 
1) to integrate floors data of building from data files (csv)
2) to calculate GFA and store in the citydb
3) to query Gross Plot Ratio (GPR) and calculate reference GFA from land plot data for visualisation

### 1.1 Requirements
1) The agent works with 3D buildings uploaded from CityGML data, follow the instructions in the [stack-data-uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#citydb-data)'s README.

2) Building address data from OSM agent
3) Linking (iri) of building and land plot by Building Identification agent

## 2. Building the Agent
The agent is designed for execution through a Docker container. Other deployment workflows are beyond the scope of this document. Follow the steps below to build and deploy the agent.
### 2.1 Preparation
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

### 2.2 Docker Deployment
In gfaagen.json, specify the database name and csv file path in environment variable, an example in the following:
```
"Env": [
    "DATABASE=postgres",
    "floors_csv=/data/HDBfloors/HDBPropertyInformation.csv"
]
```
### 2.3 Retrieving OSMAgent's image
The GFAagent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/gfaagent) using `docker pull ghcr.io/cambridge-cares/gfaagent:<LATEST-VERSION>`

### 2.4 Starting with the stack-manager
The agent has been implemented to work in the stack. To do so, place gfaagent.json in the [stack-manager config directory]. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 2.5 Running the Agent
The agent is reachable at two endpoints:
1) `/floors`: integrate floors data to citydb
2) `/calculation`: calculate GFA

No request parameters is needed.

To run the agent, run the following cURL command:
```
curl -X POST localhost:3838/gfaagent/floors
curl -X POST localhost:3838/gfaagent/calculation
```

## 3. TWA-VF Visualization

### 3.1 GeoSever layer query setting
place [mapbox-footprint.sql](stack-data-uploader-input-config/mapbox-footprints.sql) in the [stack-data-uploader config directory]. It aims to query GFA and reference GFA for visualisation.
### 3.2 Setting up TWA-VF
1) Place [`data.json`](stack-manager-input-config/data/webspace/data.json) inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation) in the stack-manager.
