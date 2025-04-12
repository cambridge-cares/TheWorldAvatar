# NTUData Agent

The purpose of the NTUDataAgent is to create 'synthetic' active and reactive load data for a desired date from the available historical NTU load data. 
This allows the test case to be run with real weather and solar PV data for any date. 

The current version of this agent simply copies load data from an appropriate historical week day i.e. it attempts to account for a weekly schedule.
The following dates with associated weekdays are used:

```
Monday:		2020-12-01 (actually a Tuesday)
Tuesday:	2020-09-01
Wednesday:	2020-04-01
Thursday:	2020-10-01
Friday:		2020-11-01 (actually a Sunday)
Saturday:	2020-02-01
Sunday:		2020-03-01
```

## Prerequisites

For the agent to produce data, the NTU power system knowledge graph must be instantiated by the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent)

## Build and deploy

The agent is designed to be only be deployed in the stack

### 1. Configure 

Provide a list of bus numbers in `config/buses.py`

### 2. Build the image

Then build image with:
```
docker build -t ntu-data-agent:1.0.0 .
```

The Dockerfile will automatically copy all properties files and mapping folders and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.

### 3. Add config to stack manager

Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `stack-manager-config/ntu-data-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "ntu-data-agent",
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
   |_ ntu-data-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager#adding-custom-containers).


### 4. Spin up a Docker Stack
**Note: The docker container must run within the same stack as the HistoricalNTUEnergyAgent to get access and query the NTU Power Network Knowledge Graph for calculation.**

Running this agent in a docker stack can facilitate interactions between other agents and endpoints (Postgres, Blazegraph, etc,.) for deployment and visualization. The stack is spun up by [Stack Manager](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager).

## Run
Once the stack is up and running, the agent can be activated by sending a Curl request as shown below with the paramters stack=true and the desired date:
```
curl -X GET "http://localhost:3838/ntu-data-agent/generate?stack=true&date=2024-03-01"
```
If the agent run successfully, you should see a JSON Object returned 
```
"Success!"
```
This indicates that the agent has queried data from the NTU Power Network Knowledge Graph and updated the KG with load data for the requested date.