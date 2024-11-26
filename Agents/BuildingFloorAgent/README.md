# Building Floor Agent
## 1. Description
This agent has been developed to improve the number of floors for 3D buildings. Presently, there are three categories number of floors:
1) Cat. A: the accurate data from reliable data source, such as government agency
2) Cat. B: the data from open data source without accuracy guarantee, such as OpenStreetMap
3) Cat. C: the data is estimate calculated by the height of buildilng

### 1.1 Requirements
The agent requires 3D building models based on the CityGML standard. These models must be uploaded through the [stack-data-uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#citydb-data).

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

### 2.2 Data requirements:

When executing the task to instantiate the number of floors from other data sources, please ensure the following data and steps are undertaken.

1. Number of floors from a specified csv file

An example of the [HDBPropertyInformation.csv](https://www.dropbox.com/scl/fi/3pgkir5zfcbhq8dliv1kr/HDBPropertyInformation.csv?rlkey=5lmb49cjqgvyrx7rcxtos1l41&dl=0) file is the HDB properties data from [Data.gov.sg](https://beta.data.gov.sg/collections/150/datasets/d_17f5382f26140b1fdae0ba2ef6239d2f/view). The csv file must contain the `blk_no`, `street`, and `max_floor_lvl` columns for this task to perform successfully. 

The csv file path and database name should be specified as environment variables in the `buildingfloor.json` file. See below for an example:
```
"Env": [
    "DATABASE=postgres",
    "floors_csv=/resources/HDBPropertyInformation.csv"
]
```
Make sure the file is available via a bind mount.
2. OpenStreetMap linked to the 3D CityGML Buildings

Please upload the OpenStreetMap following the [OSM Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OSMAgent)'s instructions and link their building IRIs by running the OSM Agent. This agent requires both steps, as it will conduct fuzzy matching between the address in the specified csv file and the address in the OpenStreetMap data. When matched, the building floor agent can instantiate the number of storeys from the csv file into the right building data in the citydb schema with the matching building IRI.

3. Estimated calculation

According to the general standard of Singapore, there are two cases:
1) Non-domestic building: the estimated floor height is 3.3m
2) Domestic building: 1st floor height is 3.6m, the rest floor height is 2.8m

### 2.3 Retrieving BuildingFloorAgent's image
The Building Floor Agent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/buildingflooragent) using `docker pull ghcr.io/cambridge-cares/buildingflooragent:<LATEST-VERSION>`

### 2.4 Starting with the stack-manager
The agent has been implemented to work in the stack. To do so, place buildingfloor.json in the [stack-manager config directory]. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 2.5 Running the Agent
To run the agent, run the following cURL command:
```
curl -X POST localhost:3838/buildingflooragent/
```

