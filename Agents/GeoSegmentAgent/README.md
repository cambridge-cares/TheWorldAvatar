# GeoSegment Agent

This agent is designed to run neural network-based segmentation algorithm to segment raster data input and create a virtual knowledge graph using Ontop and OBDA mapping. 



## 3. Build and Run
The agent is designed to run in two modes, either as a standalone docker container or work within a stack. The build and setup procedures are different for different running modes.

####  [Option 1] As a standalone docker container
Before running the agent in a standalone docker container, it is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL. However, it is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

Once a triple store and a Postgres database has been set up, modify the  `dataIRIs.properties`, `model_parameters.properties` and `ts_client.properties` in the resources folder accordingly. Refer to the property file descriptions for more information.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

####  [Option 2] As a stacked docker container

Running this agent in a docker stack is a more advanced option as it facilitate interactions between other agents for deployment and visualization. The stack is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

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
- GeoSegmentAgent
- FeatureInfoAgent (Optional)
  Note: The FeatureInfoAgent is optional and is only required if you want to visualize the result via DTVF.

##### Build the image
First, build image with:
```
docker build -t geo-segment-agent:1.0.0 .
```
The Dockerfile will automatically copy all properties files and mapping folder and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.


##### Add Config to Stack Manager
Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `./GeoSegmentAgent/stack_manager_config/geosegment-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "geosegment-agent",
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
   |_ geosegment-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

##### Spin Up Stack
Follow the [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack.

##### Run the agent
