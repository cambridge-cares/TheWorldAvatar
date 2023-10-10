# NTUEnergyCluster Agent

# 1. Prerequisites
This agent is designed to predict the Va (Voltage Angle) and Vm (Voltage Magnitude) values from both power readings and related electric parameters queried from the knowledge graph. The prediction is calculated using a feed-forward neural network model trained by the actual power grid data provided by the [Clean Energy Research Lab(CERL), NTU](http://eeeweba.ntu.edu.sg/power_projects/ntu-ONRG/0_default.asp).

For the agent to read data, three key components must be provided. The details of generating the key components are instructed in the following sections.
- An NTU power system knowledge graph instantiated by the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1496-dev-instantiate-historic-ntuenergyconsumptiondata-2/Agents/HistoricalNTUEnergyAgent).
- A trained neural network model file in `.h5` format.
- A k-means clustering model in `.pkl` format.

#### NTU Power System Knowledge Graph
This knowledge graph is used to define the relationships between individual concepts in a power system and store both static and time-series readings to simulate a 13-bus power system at NTU. All values required to run this agent should be queried from the NTU Power System Knowledge Graph using the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) from the JPS_BASE_LIB to interact with both the knowledge graph and database.
- For details to instantiate the NTU Power System Knowledge Graph, pleaes refer to the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1496-dev-instantiate-historic-ntuenergyconsumptiondata-2/Agents/HistoricalNTUEnergyAgent).
- Both NTUEnergyClusterAgent and HistoricalNTUEnergyAgent should run in the same stack in which they interact with the same Blazegraph and Postgres endpoints. For details to spin up a stack, please refer to the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

#### Neural Network Model
The NTUEnergyClusterAgent predicts Vm and Va values based on historical power data using a [feed-forward neural network](https://en.wikipedia.org/wiki/Feedforward_neural_network).
- For details of the model structure and training process, please refer to [NN_training](empty for now).
  The trained `.h5` model file should be placed under `models` folder for the agent to run.


#### K-means Clustering Model
To further facilitate optimal power flow application and visualisation, we classify the power system nodes into groups based on the predicted voltage magnitude. This is achieved via a k-means clustering model trained together with the neural network using the same set of training data.
- For details of the K-means training process and hyper-parameters, please refer to [NN_training](empty for now).
- The trained `.pkl` model file should be placed under `models` folder before running the agent.

# 2. Build & Run
This part of the README explain the instruction to build the agent.
This agent is designed to run ONLY in a docker stack. No standalone option is currently available. The following sections will explain the detailed instructions to run the agent in either mode.

### [Step 1] Build a Docker Container
The NTUEnergyClusterAgent is set up to use the Maven repository. You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
`repo_username.txt` should contain your Github username, and `repo_password.txt` your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a scope that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Then build image with:
```
docker build -t ntu-energy-cluster-agent:1.0.0 .
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
**Note: The docker container must run within the same stack as the [HistoricalNTUEnergyAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1496-dev-instantiate-historic-ntuenergyconsumptiondata-2/Agents/HistoricalNTUEnergyAgent) to get access and query the NTU Power Network Knowledge Graph for calculation.**

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
```
curl -X GET "http://localhost:3838/ntuenergycluster-agent/?stack=true"
```
If the agent run successfully, you should see a JSON Object returned back that is similar to the one shown below.
```
Successfully calculated Vm and Va.
```
This indicates that the agent has queried required data from the NTU Power Network Knowledge Graph, and updated the database with the predicted Vm and Va values. . 
