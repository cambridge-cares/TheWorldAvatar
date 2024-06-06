# NTUEnergyCluster Agent

This agent is designed to group power network buses by voltage: under-, over- and nominal voltage. Two methods are coded:
- BNN: A bubble neural network developed by the Clean Energy Research Lab(CERL), NTU to predict voltage groups given the power load for variable consumer buses. Load values are read for the KG for the request date and time. 
- BNN with P2P: As above but this receives the power consumption values of peer-to-peer trading as input and uses a neural network to predict the voltage group of each bus in the power network. 
- OPF: This method reads the voltage values for the network (calculated by the OPF Agent) from the KG and assigns voltage groups.

The agent returns the voltage groups (0: under, 1: nominal, 2: over) intended for visualisation.

For the agent to process opf results a power system must be instantiated by the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent).

#### NTU Power System Knowledge Graph
- For details to instantiate the NTU Power System Knowledge Graph, pleaes refer to the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent).
- Both NTUEnergyClusterAgent and HistoricalNTUEnergyAgent should run in the same stack in which they interact with the same Blazegraph and Postgres endpoints. For details to spin up a stack, please refer to the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

# 2. Build & Run
This part of the README explain the instruction to build the agent.
This agent is designed to run ONLY in a docker stack. No standalone option is currently available. The following sections will explain the detailed instructions to run the agent in either mode.

## Prerequisites

For bubble neural network (BNN) operation: 
- xlsx file with the trained BNN model saved to NTUEnergyClusterAgent/models/BNN_model.xlsx
- a list of the consumer bus numbers participating in P2P trading saved to NTUEnergyClusterAgent/models/buses.py

Timeseries data is expected to be in the database "ntuenergy". This can be configured in NTUEnergyClusterAgent/stack_utils/stack_config.py

### [Step 1] Build a Docker Container

Then build image with:
```
docker build -t ntu-energy-cluster-agent:2.0.0 .
```
The Dockerfile will automatically copy all properties files and mapping folders and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.

### [Step 2] Add Config to a Stack Manager
Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `stack-manager-config/ntuenergycluster-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "ntuenergycluster-agent",
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
   |_ ntuenergycluster-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).


### [Step 3] Spin up a Docker Stack
**Note: The docker container must run within the same stack as the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) to get access and query the NTU Power Network Knowledge Graph for calculation.**

Running this agent in a docker stack can facilitate interactions between other agents and endpoints (Postgres, Blazegraph, etc,.) for deployment and visualization. The stack is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).
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
- **NTUEnergyClusterAgent**
- FeatureInfoAgent (Optional)
  Note: The FeatureInfoAgent is optional and is only required if you want to visualize the result via DTVF.

### [Step 4] Run

Once the stack is up and running, the agent can be activated by sending a Curl request as shown below:

1. BNN operation with datetime parameter:
```
curl -X GET "http://localhost:3838/ntuenergycluster-agent/?stack=true&datetime=2024-02-29T17:00:00Z"
```

2. BNN with P2P operation with p2p values:
```
curl -X GET "http://localhost:3838/ntuenergycluster-agent/?stack=true&p2p=[0.0957, 0.0944, 0.0803, 0.02629, 0.0308, 0.00268]"
```

3. OPF operation with datetime parameter and opf=true
```
curl -X GET "http://localhost:3838/ntuenergycluster-agent/?stack=true&datetime=2024-03-01T02:00:00Z&opf=true"
```

If the agent run successfully, you should see a JSON Object returned with a list of voltage cluster values for all buses in the network.