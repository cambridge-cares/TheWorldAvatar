# NTU P2P Energy Agent

This agent is a Python implementation of the P2P trading algorithm developed by the [Clean Energy Research Lab(CERL), NTU](http://eeeweba.ntu.edu.sg/power_projects/ntu-ONRG/0_default.asp). The original code was implemented in MATLAB.

The agent get the solar PV generation (sellers) and load values (buyers) from the KG.

## Prerequisites

This agent depends on Gurobi for buyer and seller optimisation.  https://www.gurobi.com/solutions/gurobi-optimizer/

A power system must be instantiated by the HistoricalNTUEnergyAgent. OpenMeteo and PVLib are used to generate seller data in the form of PV power generation.
Timeseries data is expected to be in the database "ntuenergy". This can be configured in NTUEnergyClusterAgent/stack_utils/stack_config.py

The buyer and seller bus numbers must be configured in config/buses.py . Seller are buses with solar PVs.

# 2. Build & Run
This part of the README explain the instruction to build the agent.
This agent is designed to run ONLY in a docker stack. No standalone option is currently available. The following sections will explain the detailed instructions to run the agent in either mode.

### [Step 1] Build a Docker Container

List buyer and seller bus numbers in config/buses.py

Then build image with:
```
docker build -t ntu-p2p-energy-agent:1.0.0 .
```
The Dockerfile will automatically copy all properties files and mapping folders and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.

### [Step 2] Add Config to a Stack Manager
Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `stack-manager-config/ntup2penergy-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "ntup2penergy-agent",
      // Other agents you wish to spin up...
    ],
    "excludes": [
      // ...
    ]
  }
}
```

After this step, the stack-manager/inputs/config folder will have the following structure:
```
config/
|_ services/
   |_ ntup2penergy-agentt.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager#adding-custom-containers).


### [Step 3] Spin up a Docker Stack
**Note: The docker container must run within the same stack as the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) to get access and query the NTU Power Network Knowledge Graph for calculation.**

Running this agent in a docker stack can facilitate interactions between other agents and endpoints (Postgres, Blazegraph, etc,.) for deployment and visualization. The stack is spun up by [Stack Manager](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager).
A successful setup will result in 10 containers (optional 11):
- Default containers
  - Stack Manager (exits when spins up all other containers)
  - Blazegraph
  - Nginx
  - Postgis
  - Adminer
  - Ontop
  - Gdal
  - Geoserver
- **HistoricalNTUEnergyAgent (run this to instantiate NTU Power Network Knowledge Graph)**
- **NTUEnergyClusterAgent** (deploy this to run bubble neural network on P2P trading results)
- **OpenMeteoAgent** (deploy for weather data to estimate solar generation)
- **PVLibAgent** (deploy to calculate solar power generation for PVs in the power network)
- **NTUP2PEnergyAgent**
- FeatureInfoAgent (Optional)
  Note: The FeatureInfoAgent is optional and is only required if you want to visualize the result via DTVF.

### [Step 4] Run

Once the stack is up and running, the agent can be activated by sending a Curl request as shown below:

1. BNN operation with datetime parameter:
```
curl -X GET "http://localhost:3838/ntu-p2p-energy-agent/?stack=true&datetime=2024-03-01T04:00:00Z"
```
Note load timeseries and PV generation timeseries must exist for the requested datetime for the agent to run successfully.

If the agent run successfully, you should see a JSON Object returned with a list of traded power consumption values for the buyer buses that can be passed to the NTUEnergyClusterAgent.