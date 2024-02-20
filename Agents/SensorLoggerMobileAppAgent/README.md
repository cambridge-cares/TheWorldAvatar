# SensorLoggerMobileAppAgent
## 1. Description
The SensorLoggerMobileAppAgent is an agent which receives HTTP POST requests containing JSON payload sent from the [SensorLogger](https://github.com/tszheichoi/awesome-sensor-logger) mobile application, subsequently instantiate it as time series following the [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) ontology. The information instantiated from SensorLogger includes: Smartphone device, Acceleration vector, Gravity vector, Magnetic flux density vector, Sound pressure level, Illuminance, Relative brightness, Location. 

The agent functions as below:
1) The agent receives JSON payload from the SensorLogger and parse the received JSON Array.
2) It downsamples the received timeseries data via the [Downsampling](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/downsampling) library, and instantiates the data into blazegraph using the [TimeSeriesClient](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries). 
3) The [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) relations associated to each timeseries IRI are then instantiated using the [Object Graph Mapper](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/ogm) library.

## 2. Pre-requisites
### 2.1 SensorLogger mobile application setup 
The SensorLogger mobile application can downloaded either from [GooglePlay](https://play.google.com/store/apps/details?id=com.kelvin.sensorapp&hl=en&gl=US) or [IOS](https://apps.apple.com/us/app/sensor-logger/id1531582925), depending on the operating mobile OS. The steps to configure the endpoints in SensorLogger mobile app can be found [here](https://github.com/tszheichoi/awesome-sensor-logger#Live-Data-Streaming), which is summarized as below:
1) Enable HTTP PUSH under settings 
2) Specify PUSH URL following `http://<LOCAL-URL>:3838/SensorLoggerMobileAppAgent/update`
3) Replace `<LOCAL-URL>` with the same network connected from both your local environment and your phone. `<LOCAL-URL>` can be obtained from the IPv4 Address under Wireless LAN adapter Wi-Fi of your server by running `ipconfig` on command prompt.

### 2.2 Access Agent setup
The agent has been implemented to work with [Access Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent), the steps to set up Access Agent as part of the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent#spinning-up-the-access-agent-as-part-of-a-stack) which is summarized as below:
1) Replace the `STACK-NAME` with your intended stack-name in the [access-agent.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/AccessAgent/access-agent-dev-stack/access-agent.json) file.
2) Place the [access-agent.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/AccessAgent/access-agent-dev-stack/access-agent.json) in the [stack-manager config directory].

## 3. Agent Configuration 
#### Downsampling frequency
The downsampling method and frequency for the different measurements can be configured in [config.properties](sensorloggermobileappagent/src/main/resources/config.properties).
- `DSResolution` sets the time interval in seconds of the timeseries data to be downsampled.
- `DStype` refers to the downsampling method used which the enum types can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/downsampling#downsampling-type). 

#### Timer Delay and Timer Frequency
- `timerDelay` sets the initial time delay in seconds before the first timeseries instantiation.
- `timerFrequency` sets the time in seconds between each subsequent timeseries instantiation.

## 4. Deploy 
### 4.1 Retrieving SensorLoggerMobileAppAgent's image
The SensorLoggerMobileAppAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/sensorloggermobileappagent) using `docker pull ghcr.io/cambridge-cares/sensorloggermobileappagent:<LATEST-VERSION>`

### 4.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the SensorLoggerMobileAppAgent Docker container to be deployed in the stack. To do so, place [sensorloggermobileappagent.json](stack-manager-config/inputs/config/services/sensorloggermobileappagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 4.3 Configuring AccessAgent endpoints 
On this same directory run, replace `STACK-NAME` with your stack-manager name.
```
./copy.sh start <STACK-NAME>
```

### 4.4 Start recording
Once all the configurations and server has been set, press the Start Recording button inside the SensorLogger mobile app to begin session. 

## 5. Build and debug
## 5.1 Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 5.2 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the SensorLoggerMobileAppAgent local Docker Image. 

### 5.2 Spinning up with stack-manager
To debug the agent, replace [`sensorloggermobileappagent-debug.json`](stack-manager-config/inputs/config/services/sensorloggermobileappagent-debug.json) instead of [`sensorloggermobileappagent.json`](stack-manager-config/inputs/config/services/sensorloggermobileappagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

### 5.3 Testing resources
You may use the [SamplePOST request](sensorloggermobileappagent/src/main/resources/SamplePOST.http) for testing any changes made to the code, this HTTP request contains a sample of the recording for testing purposes.

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services