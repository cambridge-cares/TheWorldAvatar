# Sensor Module
- [Sensor Module](#sensor-module)
  - [Architecture Design](#architecture-design)
  - [Sensor recording: normal flow](#sensor-recording-normal-flow)
  - [Sensor recording: handling unsent data](#sensor-recording-handling-unsent-data)
    - [UnsentData table design](#unsentdata-table-design)

The module responsible for all sensor recording logics, including:
- Start/stop sensors
- Get data from sensors
- Send collected data in batches to server
- Store collected data locally as backup copy
- Record the current state of sensor recording
- Run the sensor recording, sending and storing logic as foreground service


## Architecture Design
Refer to the following diagram for the dependencies between classes in sensor module.

```mermaid
flowchart TD
    subgraph feature_user["feature:user"]
        ssf[SensorSettingFragment]
    end

    subgraph feature_user_vm["feature:user:viewmodel"]
        svm[SensorViewModel]
    end

    subgraph core_sensor_data["core:sensor:data"]
        upr[UserPhoneRepository]
        scsmr[SensorCollectionStateManagerRepository]
        sr[SensorRepository]
    end

    subgraph core_sensor_source["core:sensor:source"]
        
        subgraph core_sensor_source_state[core:sensor:source:state]
            scsm[SensorCollectionStateManager]
        end

        subgraph core_sensor_source_database[core:sensor:source:database]
            sls[SensorLocalSource]
            dao[DAOs and Entities...]
        end
        
        subgraph core_sensor_source_handler[core:sensor:source:handler]
            sm[SensorHandlerManager]
            handler[Sensor Handlers...]
        end

        subgraph core_sensor_source_activity[core:sensor:source:activity]
            arr[ActivityRecognitionReceiver]
        end
        

        subgraph core_sensor_source_network[core:sensor:source:network]
            upns[UserPhoneNetworkSource]
            sns[SensorNetworkSource]
            ncr[NetworkChangeReceiver]
        end

        subgraph core_sensor_source_worker[core:sensor:source:worker]
            bfw[BufferFlushWorker]
            suw[SensorUploadWorker]
            uduw[UnsentDataUploadWorker] 
        end
    end

    lr[LoginRepository]



    subgraph core_sensor["core:sensor"]
        ss[SensorService]
    end

    %% --- Dependencies ---
    ssf --> svm

    svm --> upr
    svm --> scsmr
    svm --> sr

    upr --> lr
    upr --> upns
    upr --> scsmr
    
    scsmr --> scsm
    scsmr --> lr

    sr --> scsmr
    sr --> sls
    
    ss --> scsmr
    ss --> sns
    ss --> ncr
    ss --> sm
    
    sm --> handler

    sns --> sls

    arr --> sls

    ncr --> sns
    ncr --> sls

    bfw --> sm
    bfw --> sls

    suw --> sns
    suw --> sls

    uduw --> sns
    uduw --> sls

    sls --> dao


    classDef UserPhone fill:#BBDEFB,stroke:#1E88E5;
    class upns,upr UserPhone

    classDef Network fill:#C8E6C9,stroke:#388E3C;
    class ncr,sns,suw,uduw Network

    classDef Local fill:#E1BEE7,stroke:#8E24AA;
    class bfw,sls,dao Local

    classDef DataCollection fill:#FFCC80,stroke:#F57C00;
    class arr,sm,handler DataCollection

    classDef StateManagement fill:#FFCDD2,stroke:#C62828;
    class scsm,scsmr StateManagement

    classDef TriggerRecording fill:#B2DFDB,stroke:#00897B;
    class sr,ss TriggerRecording
```
- $\textcolor{#00897B}{\text{Recording process}}$: related to recording process (foreground service declaration, start/stop the service)
- $\textcolor{#388E3C}{\text{Network functions}}$: related to network functions for sensor data. Includes uploading data to remote server and detecting device network changes
- $\textcolor{#8E24AA}{\text{Local storage}}$: related to app local storage
- $\textcolor{#F57C00}{\text{Sensor data collection}}$: related to sensor data collection. Deals with the actual sensors and APIs that provide data
- $\textcolor{#C62828}{\text{Recording state management}}$: mainly used by UI. Includes state such as currently selected sensors, whether the app is recording data, 'device' ID, and foreground service task ID
- $\textcolor{#1E88E5}{\text{Phone ID and User ID registration}}$: registers 'device' ID to user ID

### UI level
- SensorSettingFragment: A fragment class in `:feature:user` which provides the user interface for user to control the sensor recording
- SensorViewModel: A viewmodel class in `:feature:user:viewmodel` which handles the communication between UI and repository

### $\textcolor{#00897B}{\textbf{Recording process}}$
- SensorRepository: A repository-level component that provides control of sensor collection to UI-level components.  
- SensorService: A foreground service that keeps data collection, sending, and storage running. It triggers the sending and storing of data. [Foreground services](https://developer.android.com/develop/background-work/services/fgs) make it less likely to be terminated by the Android OS.  

### $\textcolor{#388E3C}{\textbf{Network functions}}$
- SensorNetworkSource: A network source responsible for sending collected data to the remote server.  
- SensorUploadWorker: Handles periodic data uploads to the server.  
- NetworkChangeReceiver: Monitors network connectivity and handles unsent data when the network is restored.  
- UnsentDataUploadWorker: Handles reuploading of unsent data when the device is back online.  

### $\textcolor{#8E24AA}{\textbf{Local storage}}$
- SensorLocalSource: A local source that stores collected data in the local database.  
- BufferFlushWorker: Handles periodic flushing of data from memory to local storage.  

### $\textcolor{#F57C00}{\textbf{Sensor data collection}}$
- SensorManager: A class that manages all sensor handlers.  
- Sensor handlers: Classes that manage physical sensors and collect data from them.  
- ActivityRecognitionReceiver: A class that manages and collects activity data from the [Google Activity Recognition API](https://developers.google.com/location-context/activity-recognition).  

### $\textcolor{#C62828}{\textbf{Recording state management}}$
The recording states are logged to SharedPreference files. Each file uses a hashed user ID as its filename to differentiate state files generated by different accounts on the same device. The same file also stores other app preferences and states.  
- SensorCollectionStateManagerRepository: A repository-level component that provides `SensorCollectionStateManager` functions to higher-level components or other repositories. It executes provided functions with the requested `SensorCollectionState` and clears it afterward.  
- SensorCollectionStateManager: A source class that reads and writes encrypted sensor collection state with user information to local storage.  

### $\textcolor{#1E88E5}{\textbf{Phone ID and user ID registration}}$
- UserPhoneRepository: A repository-level component that provides access to `UserPhoneNetworkSource` for the UI layer. It registers the device ID to the user ID.  
- UserPhoneNetworkSource: A network source that registers the current phone to the logged-in user.  


## Sensor recording: normal flow
This section shows the normal flow of sensor recording.

The diagram below shows SensorViewModel trigger start recording. 
```mermaid
sequenceDiagram
    participant SensorViewModel
    participant SensorRepository
    participant SensorCollectionStateManagerRepository
    participant SensorLocalSource

    SensorViewModel ->> SensorRepository : start recording
     note over SensorRepository: perform async call on SensorCollectionStateManagerRepository
    SensorRepository ->> SensorViewModel: UI thread return

    rect rgba(187,222,251,0.2)
    note over SensorRepository: run in call backs, so the UI thread is not blocked
    SensorRepository ->> SensorCollectionStateManagerRepository: set selected sensor in memory <br/> and save to SharedPreference
    SensorRepository ->> SensorLocalSource : init local database
        SensorRepository ->> SensorCollectionStateManagerRepository: set recording state in memory <br/>and save to SharedPreference
    SensorRepository ->> SensorRepository : create and start SensorService
    end
    
```
When start recording is triggered, SensorRepository performs async call to SensorCollectionStateManagerRepository to prepare to start the SensorService. UI thread is returned after the aysnc call to prevent non-responding UI, and the main logic of starting service is done in the callback of the async call. 

After the SensorService is started, it is run on separate thread and its life cycle is managed by the app and OS (not by the SensorRepository). 

The following diagram shows the work done by SensorService on `onStartCommand()` after it is started.
```mermaid
sequenceDiagram
    participant SensorService
    participant NetworkChangeReceiver
    participant SensorHandlerManager
    participant ActivityRecognitionReceiver
    participant BufferFlushWorker
    participant SensorUploadWorker
    participant SensorCollectionStateManagerRepository
    participant SensorLocalSource
    participant SensorNetworkSource

    note over SensorCollectionStateManagerRepository: SensorCollectionStateManagerRepository is mainly used by SensorService to get the taskID<br/> when performing upload and network status monitoring. It is omitted from the diagram for simplicity.

    note over NetworkChangeReceiver: The following code happens in onStartCommand() lifecylce function,<br/> when SensorService is started.
    SensorService ->> NetworkChangeReceiver: register network change receiver
    
    activate NetworkChangeReceiver
    NetworkChangeReceiver ->> NetworkChangeReceiver: listen to change in device network state

    SensorService ->> SensorHandlerManager : start selected sensors
    note over SensorHandlerManager: Selected sensor handlers are activated

    alt record activity recognition
    SensorService ->> ActivityRecognitionReceiver: register ActivityRecognitionReceiver
    activate ActivityRecognitionReceiver
    end

    SensorService ->> BufferFlushWorker: schedule buffer flush task
    activate BufferFlushWorker
    rect rgba(135,206,250,0.2) 
        note over BufferFlushWorker: Sample workflow of BufferFlushWorker
        loop Every buffer_delay ms
            BufferFlushWorker ->> SensorHandlerManager : collect sensor data
            BufferFlushWorker ->> SensorLocalSource: write to local data source 
            BufferFlushWorker ->> SensorLocalSource: delete local data older than 30 days
        end
    end

    SensorService ->> SensorUploadWorker: schedule sensor upload task
    activate SensorUploadWorker
    rect rgba(255,182,193,0.2)
        note over SensorUploadWorker: Sample workflow of SensorUploadWorker
        loop Every upload_delay ms
            loop when all pages has been uploaded
                SensorUploadWorker ->> SensorLocalSource : retrieve unuploaded sensor data from individual sensor tables with pagination
                SensorUploadWorker ->> SensorUploadWorker: compress data 
                SensorUploadWorker ->> SensorNetworkSource: send compressed data
            end
        end
    end
    
    note over NetworkChangeReceiver: All processes are kept active and running at their own frewuencies.<br/> They are terminated when the SensorService is been stopped<br/> and the termination happens in onDestroy() lifecycle function
    deactivate NetworkChangeReceiver
    deactivate ActivityRecognitionReceiver
    deactivate BufferFlushWorker
    deactivate SensorUploadWorker

```


## Sensor recording: handling unsent data
This section shows the process that occurs when sensor data fails to upload to the remote server (e.g., due to a network issue or service downtime). This functionality ensures that no data is lost if a user loses
connection temporarily or goes offline by storing the unsent data locally and uploading it when the connection is restored.

The below diagram illustrates the case when data is not sent successfully.
```mermaid
sequenceDiagram 
    participant SensorUploadWorker
    participant SensorNetworkSource
    participant SensorLocalSource

    SensorUploadWorker ->> SensorLocalSource : retrieve data from individual sensor tables
    SensorUploadWorker ->> SensorNetworkSource : send post request with compressed data
    SensorNetworkSource ->> SensorNetworkSource : fail to send data to remote server
    SensorNetworkSource ->> SensorLocalSource : add the data to `unsent table`
```
Fresh data collected by sensors or APIs are stored in seperate sensor tables (eg. acceleration table, location table etc.) and retrieved by `SensorUploadWorker` under normal conditions. Data that fail to upload are moved to the `unsent table` in the call back of post request in `SensorNetworkSource`.

The following diagram shows the process when the device is back online.
```mermaid
sequenceDiagram 
    participant NetworkChangeReceiver
    participant UnsentDataWorker
    participant SensorUploadWorker
    participant SensorNetworkSource
    participant SensorLocalSource

    note over NetworkChangeReceiver: NetworkChangeReceiver is invoked by the app when there is change in the device network state
    NetworkChangeReceiver ->> NetworkChangeReceiver : check and find network is available
    NetworkChangeReceiver ->> UnsentDataWorker : create UnsentDataWorker as one time work request and add to WorkManager queue

    UnsentDataWorker ->> SensorLocalSource : retrieve unsent data from `unsent table` with pagination
    UnsentDataWorker ->> NetworkChangeReceiver: compress unsent data
    UnsentDataWorker ->> SensorNetworkSource : send post request
    UnsentDataWorker ->> SensorLocalSource : delete unsent data  
```
NOTICE: 
- The current way of deleting unsent data isn't done in the call back of SensorNetworkSource. For the current employment, the unuploaded data will be added by SensorNetworkSource if it fails to upload so there won't be data lost. The code could be optimized further so the deletion of sent data and storing of unsent data can be in the same place.

### UnsentData table design
`UnsentData table` is designed in the way to facilitate fast retrieval. Each record corresponds to an unsent request, so the data field stores the serialized string of sensor data which reduces the processing time when reconstructing the post request during resending.
| Field | Description |
|--------|-------------|
| `int id` | Autogenerated ID that uniquely identifies each row in the table. |
| `String data` | Serialized map of `AllSensorData` from `sendPostRequest` that could not be uploaded. The data is serialized into a string in the `SensorNetworkSource` class and deserialized in `NetworkChangeReceiver` before being uploaded. |
| `String deviceId` | ID associated with the device the app is operating on. |
| `long timestamp` | Timestamp indicating when the unsent data is processed in `sendPostRequest` within `SensorNetworkSource`. |
| `String dataHash` | Unique hash code used to identify the batch of unsent data. |

To minimize the number of requests during the resend process, the code combines payloads with the same device ID within the retrieved batch of unsent data. Each POST request follows the `SensorLoggerMobileAppAgent` format.
```json
{
"messageId":21,
"sessionId":"7dc8a9c4-ccd4-4961-8b2b-568f414123b4",
"deviceId":"605a09c9-d6c5-4ba7-bc28-fe595d698b41",
"payload":
  [
    {"name":"accelerometer","accuracy":3,"time":1676967401045727000,"values":{...}},
    ...
  ]
}
```
