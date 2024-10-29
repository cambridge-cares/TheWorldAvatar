# SensorLoggerMobileAppAgent
## 1. Description
The SensorLoggerMobileAppAgent is an agent which receives HTTP POST requests containing JSON payload sent from the [SensorLogger](https://github.com/tszheichoi/awesome-sensor-logger) mobile application, subsequently instantiate it as time series following the [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) ontology. The information instantiated from SensorLogger includes: Smartphone device, Acceleration vector, Gravity vector, Magnetic flux density vector, Sound pressure level, Illuminance, Relative brightness, Location. 

The agent functions as below:
1) The agent receives JSON payload from the SensorLogger and parse the received JSON Array.
2) It downsamples the received timeseries data via the [Downsampling](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/downsampling) library, and instantiates the data using the [TimeSeriesClient](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries). 
3) The [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice) triples are instantiated in Ontop.

### 1.1 Concurrency Design
The agent manages a phone ID to recording task map, where each phone ID will have a corresponding recording task. The recording task is responsible for sensor data processing, knowldge graph instantiation and postgres table initiation and data upload. Each recording task has different types of sensor processors, which are responsible for the sensor IRI query and generation, downsampling and data formulation for individual types. The following class diagram highlight the relations between class and omit some details of some classes for simlicity.

```mermaid
classDiagram
class SensorLoggerMobileAppAgent
SensorLoggerMobileAppAgent: -addDataExecutor
SensorLoggerMobileAppAgent: -sendDataExecutor
SensorLoggerMobileAppAgent: -processRequestQueue(payload)

class SmartphoneRecordingTask
SmartphoneRecordingTask: -tsClient
SmartphoneRecordingTask: -agentConfig
SmartphoneRecordingTask: -lastProcessedTime
SmartphoneRecordingTask: -lastActiveTime
SmartphoneRecordingTask: -isProcessing
SmartphoneRecordingTask: +addData(data)
SmartphoneRecordingTask: +shouldProcessData() bool
SmartphoneRecordingTask: +shouldTerminateTask() bool
SmartphoneRecordingTask: +processAndSendData()
SmartphoneRecordingTask: -initSensorProcessors()
SmartphoneRecordingTask: -bulkInitKg()
SmartphoneRecordingTask: -bulkInitRdb()
SmartphoneRecordingTask: -bulkAddTimeSeriesData()

class AgentConfig
class EndpointConfig
class OntoConstants
class StaticInstantiation

class SensorDataProcessor
SensorDataProcessor: -storeClient
SensorDataProcessor: -smartphoneIRINode
SensorDataProcessor: -timeList
SensorDataProcessor: -isIriInstantiationNeeded
SensorDataProcessor: -isIriInstantiationNeeded
SensorDataProcessor: +addData(data)
SensorDataProcessor: +getProcessedTimeSeries() TimeSeries<OffsetDateTime>
SensorDataProcessor: +initIRIs()
SensorDataProcessor: +getDataClass() List<Class>
SensorDataProcessor: +getDataIRIMap() Map<String, String>
SensorDataProcessor: +getTimeSeriesLength() int
SensorDataProcessor: clearData() 
SensorDataProcessor: getIrisFromKg()

class AccelerometerProcessor
AccelerometerProcessor: -xIri
AccelerometerProcessor: -yIri
AccelerometerProcessor: -zIri
AccelerometerProcessor: -xList
AccelerometerProcessor: -yList
AccelerometerProcessor: -zList

class DBFSDataProcessor
DBFSDataProcessor: -dbfsIRI
DBFSDataProcessor: -dBFSList

class GravityDataProcessor
GravityDataProcessor: -xIri
GravityDataProcessor: -yIri
GravityDataProcessor: -zIri
GravityDataProcessor: -xList
GravityDataProcessor: -yList
GravityDataProcessor: -zList

class IlluminationProcessor
IlluminationProcessor: -illuminationIri
IlluminationProcessor: -illuminationList

class RelativeBrightnessProcessor
RelativeBrightnessProcessor: -relativeBrightnessIRI
RelativeBrightnessProcessor: -brightnessList

class MagnetometerDataProcessor
MagnetometerDataProcessor: -xIri
MagnetometerDataProcessor: -yIri
MagnetometerDataProcessor: -zIri
MagnetometerDataProcessor: -xList
MagnetometerDataProcessor: -yList
MagnetometerDataProcessor: -zList


class LocationDataProcessor
LocationDataProcessor: -bearingIRI
LocationDataProcessor: -speedIRI
LocationDataProcessor: -altitudeIRI
LocationDataProcessor: -pointIRI
LocationDataProcessor: -bearingList
LocationDataProcessor: -speedList
LocationDataProcessor: -altitudeList
LocationDataProcessor: -geomLocationList

SensorLoggerMobileAppAgent *-- SmartphoneRecordingTask: 0..*
SensorLoggerMobileAppAgent -- AgentConfig: 1
SensorLoggerMobileAppAgent -- EndpointConfig: 1

SmartphoneRecordingTask *-- AccelerometerProcessor: 1
SmartphoneRecordingTask *-- DBFSDataProcessor: 1
SmartphoneRecordingTask *-- GravityDataProcessor: 1
SmartphoneRecordingTask *-- LocationDataProcessor: 1
SmartphoneRecordingTask *-- IlluminationProcessor: 1
SmartphoneRecordingTask *-- MagnetometerDataProcessor: 1
SmartphoneRecordingTask *-- RelativeBrightnessProcessor: 1
SmartphoneRecordingTask -- AgentConfig: 1

SensorDataProcessor <|-- AccelerometerProcessor
SensorDataProcessor <|-- DBFSDataProcessor
SensorDataProcessor <|-- GravityDataProcessor
SensorDataProcessor <|-- LocationDataProcessor
SensorDataProcessor <|-- IlluminationProcessor
SensorDataProcessor <|-- MagnetometerDataProcessor
SensorDataProcessor <|-- RelativeBrightnessProcessor
SensorDataProcessor -- AgentConfig: 1
```

To handle multi-user cases, this agent implements a task queue and thread pool model for concurrent processing. Tasks are managed by the agent. There are four types of thread used by the agent:
- Agent Main thread: The main thread is used to receive requests from clients. When a request is sent to the agent, this thread checks the validaty of the request, submit an 'Add Data' task to the 'Add Data Thread' pool and return to the request to the client. This thread **should not be used for any heavy task** to ensure fast response to clients.
- Agent Timer Thread: This thread wakes up every certain time duration to checks whether the SmartphoneRecordingTask should be processed by comparing `lastProcessedTime` and the current time. If the SmartphoneRecordingTask should be processed, this thread will submit a 'Send Data' task to the 'Send Data Thread Pool'. This task **should not be used for any heavy task** and only used for scanning the tasks map. 
- Add Data Thread Pool: This thread pool will attend to 'Add Data' tasks, which processes the raw data from request and add the data to the corresponding SmartphoneRecordingTask, whenever there is free thread available in the pool. The default number of threads in the pool is 5.
- Send Data Thread Pool: This thread pool will attend to 'Send Data' tasks, which performs data downsampling on vaious types of sensor data based on the configuration and bulk initialization, KG instantiation and upload of sensor data, whenever there is free thread available in the pool. The default number of threads in the pool is 5.

The recording task uses three states `lastProcessedTime`, `lastActiveTime` and `isProcessing` to control the process. 
- `lastProcessedTime`: This state is used to control the rate of performing data downsample and writing to postgres database
- `lastActiveTime`: This state monitors whether there were data received from a phone in a duration of time. If there isn't any data from the device, the agent will terminate the task for the device.
- `isProcessing`: If this task is already been processing by a 'Send Data' thread, other 'Send Data' threads should skip this task.

The following chart shows an example when 
- phone3 sends request to the agent and the main thread create a task for the Add Data Pool
- Timer thread wakes up and is checking whether should process the `SmartphoneRecordingTask`s. It finds that SmartphoneRecordingTask 2 needs to be processed and is creating the relevant task to the Send Data Pool.
```mermaid
stateDiagram-v2
main: Agent Main Thread
timer: Agent Timer Thread
addpool: Add Data Thread Pool
sendpool: Send Data Thread Pool

ps: Phones
state ps {
p1: Phone 1
p2: Phone 2
p3: Phone 3
}

state addpool{
add1: Add data to phone 1
add2: Add data to phone 2
}

state sendpool{
send1: Send data of phone 1
}

ts: SmartphoneRecordingTask map
state ts {
t1: SmartphoneRecordingTask 1
t2: SmartphoneRecordingTask 2
t3: SmartphoneRecordingTask 3
}

p3 --> main: Send request with data
main --> addpool: Submit task to add data to phone 3
t2 --> sendpool: Submit task to send data to phone 2
timer --> t1: Check shouldProcess
timer --> t2: Check shouldProcess
timer --> t3: Check shouldProcess

note left of timer: This thread is spun up by Agent Main Thread
```

## 2. Pre-requisites
### 2.1 SensorLogger mobile application setup 
The SensorLogger mobile application can downloaded either from [GooglePlay](https://play.google.com/store/apps/details?id=com.kelvin.sensorapp&hl=en&gl=US) or [IOS](https://apps.apple.com/us/app/sensor-logger/id1531582925), depending on the operating mobile OS. The steps to configure the endpoints in SensorLogger mobile app can be found [here](https://github.com/tszheichoi/awesome-sensor-logger#Live-Data-Streaming), which is summarized as below:
1) Enable HTTP PUSH under settings 
2) Specify PUSH URL following `http://<LOCAL-URL>:3838/sensorloggermobileappagent/update`
3) Replace `<LOCAL-URL>` with the same network connected from both your local environment and your phone. `<LOCAL-URL>` can be obtained from the IPv4 Address under Wireless LAN adapter Wi-Fi of your server by running `ipconfig` on command prompt.

## 3. Agent Configuration 
#### Downsampling frequency
The downsampling method and frequency for the different measurements can be configured in [config.properties](sensorloggermobileappagent/src/main/resources/config.properties).
- `DSResolution` sets the time interval in seconds of the timeseries data to be downsampled.
- `DStype` refers to the downsampling method used which the enum types can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/core/downsampling#downsampling-type). 

#### Timer Delay and Timer Frequency
- `timerDelay` sets the initial time delay in seconds before the first timeseries instantiation.
- `timerFrequency` sets the time in seconds between each subsequent timeseries instantiation.
- `taskInactiveTime` sets the time in seconds to remove a `SmartPhoneRecording` task when it no longer receives data from the corresponding device.

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