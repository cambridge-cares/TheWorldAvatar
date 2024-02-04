# SensorLoggerMobileAppAgent
## 1. Description
The SensorLoggerMobileAppAgent is an agent which receives HTTP POST requests containing JSON payload sent from the [SensorLogger](https://github.com/tszheichoi/awesome-sensor-logger) mobile application, subsequently instantiate it as time series, following the [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) ontology. The information instantiated from SensorLogger includes: Smartphone devuce, Acceleration vector, Gravity vector, Magnetic flux density vector, Sound pressure level, Illuminance, Relative brightness, Location. 

The agent functions as below:
1) The agent receives JSON payload from the SensorLogger mobile app and parse the received JSON Array.
2) It downsamples the received timeseries data via the [Downsampling](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/downsampling) library, and instantiates the data into blazegraph through the [TimeSeriesClient](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries). 
3) The [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) relations associated to each timeseries IRI are then instantiated using the [Object Graph Mapper](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/ogm) library.


## 2. Pre-requisites
### 2.1 SensorLogger mobile application setup 
1) The SensorLogger mobile application can downloaded either from [GooglePlay](https://play.google.com/store/apps/details?id=com.kelvin.sensorapp&hl=en&gl=US) or [IOS](https://apps.apple.com/us/app/sensor-logger/id1531582925), depending on the operating mobile OS.
2) Ensure sufficient permissions are given to record measurements.
3) Configure endpoints in SensorLogger mobile app following instructions [here](https://github.com/tszheichoi/awesome-sensor-logger#Live-Data-Streaming), which can be summarized as below:
   1) Enable HTTP PUSH under settings 
   2) Specify PUSH URL following `http://<LOCAL-URL>:10102/SensorLoggerMobileAppAgent/update`
   3) Replace `<LOCAL-URL>` with the same network connected from both your local environment and your phone. `<LOCAL-URL>` can be obtained from the IPv4 Address under Wireless LAN adapter Wi-Fi of your server by running `ipconfig` on command prompt.

### 2.2 Access Agent setup
1) Set up [JPS_Access Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent) as part of the stack, following instructions [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent#spinning-up-the-access-agent-as-part-of-a-stack).
2) Replace the `STACK-NAME` with the stack name in the [access-agent.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/AccessAgent/access-agent-dev-stack/access-agent.json) file.
3) Replace the `STACK-NAME` with the stack name in the [routing.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/AccessAgent/access-agent-dev-stack/access-agent.json) file.


## 3. Agent Configuration 
### 3.1 Downsampling frequency
1) The downsampling frequency can be configured in [config.properties](sensorloggermobileappagent/src/main/resources/config.properties).
2) Replace the `STACK-NAME` with the stack name in the [StaticInstantiation.java](sensorloggermobileappagent/src/main/java/uk/ac/cam/cares/jps/agent/sensorloggermobileappagent/StaticInstantiation.java) file, under ModelContext.

## 4. Build
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

## 5. Deploy 
### 5.1 Retrieving SensorLoggerMobileAppAgent's image
The SensorLoggerMobileAppAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/sensorloggermobileappagent) using `docker pull ghcr.io/cambridge-cares/sensorloggermobileappagent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the SensorLoggerMobileAppAgent Docker container to be deployed in the stack. To do so, place [sensorloggermobileappagent.json](stack-manager-config/inputs/config/services/sensorloggermobileappagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 AccessAgent configuation 
1) Add in `storerouter` namespace in blazegraph. This is so that AccessAgent can reach the relevant endpoint URL. 
2) Run `bash ./uploadRouting.sh` in the [accessagent_resources](accessagent_resources/) folder

### 5.4 Start recording
Once everything has been configured. Press the recording button inside the SensorLogger mobile app to begin. 

## 6. Debugging
### 6.1 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the SensorLoggerMobileAppAgent local Docker Image. 

### 6.2 Spinning up with stack-manager
To debug the agent, replace [`sensorloggermobileappagent-debug.json`](stack-manager-config/inputs/config/services/sensorloggermobileappagent-debug.json) instead of [`sensorloggermobileappagent.json`](stack-manager-config/inputs/config/services/sensorloggermobileappagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

### 6.3 Testing resources
You may use the [SamplePOST request](sensorloggermobileappagent/src/main/resources/SamplePOST.http) for testing any changes made to the code, this HTTP request contains a sample of the recording for testing purposes.

[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services