# SeaLevelImpactAgent

## 1. Description

The SeaLevelImpactAgent is an agent that
1) Receives inputs - `SSP Scenario`, `Confidence Level`, `Percentage Quantile`
2) Instantiate the sealevelimpact

## 2. Prerequisites

### 2.1. Stack Set Up

The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack. Several pre-configured examples for the different use cases for King's Lynn can be found in [stack-data-uploader-inputs](stack-data-uploader-inputs/).

## 3. Agent Configuration

### 3.1 Config Properties
1) `dbName` - Specify the postgresql database


## 4. Build

### 4.1. GitHub Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:

```bash
./credentials/
        repo_username.txt
        repo_password.txt
```

## 5. Deployment

### 5.1 Retrieving SeaLevelImpactAgent's image

The SeaLevelImpactAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/sealevelimpactagent) using `docker pull ghcr.io/cambridge-cares/sealevelimpactagent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the SeaLevelImpactAgent Docker container to be deployed in the stack. To do so, place [sealevelimpactagent.json](stack-manager-config/inputs/config/services/sealevelimpactagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent

The agent is reachable at the `/slrimpact` endpoint.

#### Input specification

1) `function` - The use case scenario to run the travelling salesman problem.

#### Urban Resilience Planning (UR)

Generates travelling salesman route via geoserver SQL view. this route runs through the points of interest and return back to the original location. The SQL view layers takes TWA-VF marker location as the target node for its calculations. The geoserver layers generated include:

- Normal wading depth capability
- 30cm wading depth capability
- 90cm wading depth capability

To run the agent, simply run the following cURL command:

```bash
curl -X POST "localhost:3838/sealevelimpactagent/runtsp?function=UR"
```

## 6. Debugging

### 6.1 Building Docker Image

In the same directory as this README, run `docker compose build`. This will build the SeaLevelImpactAgent local Docker Image.

### 6.2 Spinning up with stack-manager

To debug the agent, replace [`sealevelimpactagent-debug.json`](stack-manager-config/inputs/config/services/sealevelimpactagent-debug.json) instead of [`sealevelimpactagent.json`](stack-manager-config/inputs/config/services/sealevelimpactagent.json) in the [stack-manager config directory].

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. TWA-VF Visualisation

### 7.1 Feature Info Agent

1) In the directory [stack-manager-config/data/webspace/](stack-manager-config/data/webspace/), contains the TWA-VF `data.json` prepared for the different scnearios that is meant to be placed inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation).

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
