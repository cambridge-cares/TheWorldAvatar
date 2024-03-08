# GFAAgent
## 1. Description
This agent has been developed to support and compute the Gross Floor Area (GFA) of buildings. Presently, the agent performs the following two tasks:
1) Instantiate the number of storeys within a building from other data sources (OSM, csv) into the citydb schema
2) Calculate the Gross Floor Area of buildings through multiplying the area of the building footprint with the number of storeys

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

### 2.2 Raw data for floors data integration
In gfaagen.json, specify the database name and csv file path in environment variable. Currently it only support HDB properties data from [Data.gov.sg](https://beta.data.gov.sg/collections/150/datasets/d_17f5382f26140b1fdae0ba2ef6239d2f/view), which include building address and floors data. An example in the following:
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

