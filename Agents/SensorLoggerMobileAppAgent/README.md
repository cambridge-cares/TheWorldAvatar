# SensorLoggerMobileAppAgent
## Description
The SensorLoggerMobileAppAgent is intended to receive HTTP POST requests containing JSON payload from the [SensorLogger mobile app](https://github.com/tszheichoi/awesome-sensor-logger). The agent retrieves JSON payload from the SensorLogger mobile app, parse the JSON Array and instantiate the data onto the knowledge graph using the timeseries client. The static relations are instantiated using the object graph mapper library.

## To deploy this agent with the stack
1) Spin up the stack-manager

The agent has been implemented to work with stack, which requires the stack to be [set up](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

Before building, change the placeholder `<STACK-NAME>` in `./sensorloggermobileapp_agent/src/main/resources/config.properties` to the name of your stack. Currently, some parts hardcoded to be `test`.

2) Input the necessary credentials in the folders

You'll need to provide  your credentials in single-word text files located like this:
#### Under the main folder
```
./SensorLoggerMobileAppAgent/
    credentials/
        repo_username.txt
        repo_password.txt
```

#### Under the docker folder
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```

## Debugging the agent
#### Building the docker image 
On the Debug side panel of VSCode, run the `Build and debug` configuration. (This will fail but will produce the docker image)

#### Debugging
1) Insert breakpoints within the code.
2) On the Debug side panel of VSCode, run the `Debug` configuration.
3) It will prompt you to input the <STACK-NAME> (This will fail but will spin up the agent within the stack - `Failed to attach to remote debuggee VM. Reason: com.sun.jdi.connect.spi.ClosedConnectionException`)
4) Run `Reattach and debug` to enter the debug mode. 

## Testing the agent
1) Send the POST Request of `SamplePOST_for_Stack` in `/SensorLoggerMobileAppAgent/sensorloggermobileapp_agent/src/main/resources`. You will receive 200 status code as response.
