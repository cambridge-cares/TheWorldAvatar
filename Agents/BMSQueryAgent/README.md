# BMSQueryAgent
BMSQueryAgent is an agent designed to query for equipment instances and the related zones from the knowledge graph.
With this agent, a user can get all the equipment following the link building - facility - room - equipment. 

To achieve a balance between response speed and body size, the agent breaks the above link to two Http requests. 
- Request sent to `retrieve/zones` will return all the available buildings, the associated facilities and all the rooms in each facility in JSON format. 
- Request sent to `retrieve/lab` is similar to `retrieve/zones` but only the zones in lab namespace
- Request sent to `retrieve/office` is similar to `retrieve/zones` but only the zones in office namespace
- Once the room is determined, users can send `retrieve/equipment?RoomIRI=<selected room iri>` to get all the equipment in the selected room.

# 1. Setup
This agent is designed to run in stack, which is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). 
A successful setup will result in 9 containers (optional 10):
- Default containers
  - Stack Manager (exits when spins up all other containers)
  - Blazegraph
  - Nginx
  - Postgis
  - Adminer
  - Ontop
  - Gdal
  - Geoserver
- BMSQueryAgent
- FeatureInfoAgent (Optional)

For the BMSQueryAgent to return results, it is assumed that there is already knowledge graph in the Blazegraph.

BMSQueryAgent does not depend on [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent), but they are used together to create time series visualisation in the [BMS Query App](https://github.com/cambridge-cares/TheWorldAvatar/tree/1502-android-app-for-data-visualisation/Apps/BMSQueryApp).

## 1.1 Config BMSQueryAgent in Stack

Copy `stack-manager-input-config-service/bms-query-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.

Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it doesn't exist. If it exists already, append the agent to the file as follows:
```json
{
  "services": {
    "includes": [
      "bms-query-agent",
      // ...
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
   |_ bms-query-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

## 1.2 Spin Up Stack
Follow the [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack. 

# 2. Usage
The agent accepts three paths:
- /status
- /retrieve/zones
- /retrieve/lab
- /retrieve/office
- /retrieve/equipment?roomIRI="room iri"

## Status
This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:3838/bms-query-agent/status
```

Result in:
```json
{"description":"BMSQueryAgent is ready."}
```

## Retrieve Zones
This request gets all the available buildings from lab and office namespace, the associated facilities and all the rooms in each facility in JSON format. The request has the following format:
```
curl -X GET http://localhost:3838/bms-query-agent/retrieve/zones
```

Result in:
```json
{
    "buildings": {
        "http://www.theworldavatar.com/lab/Building_6eddf039-b309-4f3c-854a-93ef0891f646": {
            "facilities": {
                "https://www.theworldavatar.com/kg/ontodevice/CaresLabOne": {
                    "rooms": {
                        "http://www.theworldavatar.com/lab/Room_857c535d-f065-4baf-b153-85c8b63f5541": {
                            "label": "Open Lab Area"
                        },
                        ...
                    },
                    "label": "CARES Lab 1"
                },
                "https://www.theworldavatar.com/kg/ontodevice/CaresLabTwo": {
                    "rooms": {
                        "http://www.theworldavatar.com/lab/Room_965efa84-0689-4d8e-bbf6-af771f2b2b8d": {
                            "label": "Biological Containment Area"
                        },
                        ...
                    },
                    "label": "CARES Lab 2"
                }
            },
            "label": "CREATE Building Research Wing"
        }
    }
}
```

## Retrieve Lab Zones
This request gets all the available buildings from lab namespace, the associated facilities and all the rooms in each facility in JSON format. The request has the following format:
```
curl -X GET http://localhost:3838/bms-query-agent/retrieve/lab
```
The response will have similar structure as in `/retrieve/zones`

## Retrieve Office Zones
This request gets all the available buildings from office namespace, the associated facilities and all the rooms in each facility in JSON format. The request has the following format:
```
curl -X GET http://localhost:3838/bms-query-agent/retrieve/office
```
The response will have similar structure as in `/retrieve/zones`

## Retrieve Equipment
This request gets all the equipment in a given room. The request has the following format:
```
curl -X GET 'http://localhost:3838/bms-query-agent/retrieve/equipment?roomIRI=http://www.theworldavatar.com/lab/Room_ff3f935b-db1c-4be7-ae06-19c189462e89'
```

Result in:
```json
{
    "equipment": [
        {
            "iri": "https://www.theworldavatar.com/kg/ontobms/VAV_E7_18A_4a7bd117-e475-404a-b6bd-f165a5e9c9ec",
            "label": "VAV_E7_18A",
            "type": "https://www.theworldavatar.com/kg/ontobms/VAVSystem"
        },
        {
            "iri": "https://www.theworldavatar.com/kg/ontobms/WFH-09_a0ca17b8-19dd-4a4e-8005-316277539071",
            "label": "WFH-09",
            "type": "https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood"
        }
    ]
}
```
