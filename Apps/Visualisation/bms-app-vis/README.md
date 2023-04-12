# BMS Visualisation 

This is a webapp project built with [DTVF](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework). It uses the time series module from DTVF for lab equipment data visualisation and is optimised for mobile device. The web page is part of the BMS Visualisation App.

<img src="visualisation.JPG" alt="Example of lab equipment data visualisation in BMS Visualisation App" width="40%"/>

# Restrictions

It should be noted that this project doesn't use the Cesium module in DTVF, and thus makes no use of Cesium's premium offering, Cesium Ion. 


# Building the Image

This project is designed to run in a docker container and needs a [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent), [Android Status Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1517-dev-android-status-agent/Agents/AndroidStatusAgent), blazegraph and postgis database running in a stack. The web page will send a request with the iri of an equipment to Feature Info Agent, and the agent will retrieve time series data with the blazegraph and postgis database. Therefore, for this web to display the graph, data should already exist in the blazegraph and postgis database running in the stack.

## Setup Stack and Agents

Please refer to [stack manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack), [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent#deploy-the-agent) and [Android Status Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1517-dev-android-status-agent/Agents/AndroidStatusAgent#1-setup) for setup instructions.

## Setup BMS Visualisation

### Config Stack Address

Modify the stack's address in [index.html](https://github.com/cambridge-cares/TheWorldAvatar/blob/1502-android-app-for-data-visualisation/Apps/Visualisation/bms-app-vis/webspace/index.html#L50).

### Launch Container

Run the following command to build the image and launch the bms-app-vis container:
```
docker-compose -f ./docker/docker-compose.yml up -d
```

It is worth noting that the `webspace` is mounted to the container at `/var/www/html`, so any changes made to the folder after the container is up will be effective.

After a successful setup, the docker containers should have the following structure:
```
bms-app-vis
<STACK>
|_ Android Status Agent
|_ Feature Info Agent
|_ blazegraph
|_ postgis
|_ ...
```