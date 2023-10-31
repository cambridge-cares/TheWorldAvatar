# NTU Forecasting Agent

This agent is designed to deploy the forecasting agent for the NTU energy scenario. In order to produce forecasts, requests can be sent to this agent which, if necessary, instantiates triples in the knowledge graph and finally lets the forecasting agent predict the time series according to the request's specified arguments. Thus, the NTU forecasting agent can only be deployed in a stack together with the forecasting agent.

# 1. Setup

Setup the forecasting agent as part of a stack following the instructions of the [forecasting agent](https://github.com/cambridge-cares/TheWorldAvatar/edit/main/Agents/ForecastingAgent/).

In order to fill the knowledge graph with the data of the NTU scenario, the instructions for adding the [Historical NTUEnergy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) to the stack should be executed as well.


#### Build the agent

Build the image of the agent locally by executing in this folder in a terminal:
```
docker build -t ntu_forecasting_agent:1.0.0 .
```

#### Add config to the Stack Manager

Copy ```./NTUForecastingAgent/stack_manager_config/ntu-forecasting-agent.json``` to ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/```, where the  configs of the forecasting agent and historical NTUEnergy agent should be as well.

If the instructions for the forecasting agent and historical NTUEnergy agent were followed correctly, the file ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json``` should exist. Add the NTU forecasting Agent in this file in the same fashion as for the other agents. it should look like this:

```json
{
  "services": {
    "includes": [
      "historical-ntuenergy-agent",
      "forecasting-agent-debug",
      "ntu-forecasting-agent"
    ]
  }
}
```

#### Setup correct namespace

The default namespace of the forecasting agent in ```forecasting-agent.json``` is specified as ```kb```, which is not the namespace the historical NTUEnergy agent uses. Therefore, change ```NAMESPACE``` in ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/forecasting-agent.json``` to ```ntuenergy```. It should look like this:

```json
{
    "ServiceSpec": {
        "Name": "forecasting-agent",
        "TaskTemplate": {
            "ContainerSpec": {
                "Image": "ghcr.io/cambridge-cares/forecasting-agent:2.1.1",
                "Mounts": [
                    {
                        "Type": "bind",
                        "Source": "/home/common/Codes/TheWorldAvatar/Agents/ForecastingAgent/forecastingagent/fcmodels",
                        "Target": "/app/forecastingagent/fcmodels"
                    }
                ],
                "Env": [
                    "NAMESPACE=ntuenergy",
                    (...)
(...)

```

#### Spinning up

Now, follow the [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack. The stack manager should bring up 12 containers in total. Activate the [Historical NTUEnergy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) by sending the specified curl request for the agent to instantiate the knowledge graph of the NTU power network.

# 2. Usage of the agent

To use the agent, send request with the arguments for the agent. Like for the forecasting agent, the arguments the requests should include are:

```
    frequency: <IRI_of_forecast_frequency>
    forecasting_model: <IRI_of_forecasting_model>
    interval: <IRI_of_interval_to_forecast>
    duration IRI_duration: <IRI_of_historical_data_length>
    iri_to_forecast: <IRI_to_forecast>
```

Refer to the forecastig agent how the triples with these IRIs should be defined. 

If frequency, model and duration are not included in the request, pre-defined default values will be used. (frequency: 1 hour, model: prophet without scaling, duration: 336 hours)

However, the request fails if the interval or IRI_to_forecast are not defined. A forecast does not make much sense if the thing to forecast or specific interval to forecast are unknown, after all.

Alternatively, instead of specifying the IRI of an interval, a request can also contain ```time_pos1``` and ```time_pos2``` as arguments with float values. In this case, the agent will instantiate a new interval in the KG with ```time_pos1``` as start time and ```time_pos2``` as end time and use them for the forecast.

```
time_pos1: <float representing a Unix timestamp>
time_pos2: <float representing a Unix timestamp>
```

A request with the IRI_to_forecast ```https://www.theworldavatar.com/kg/ontopowsys/BusNode_6_AbsorbedReactivePower``` and IRI_of_interval_to_forecast ```http://example.org/OptimisationInterval_1``` and the pre-defined default values would look like this:

```
curl -X GET "http://localhost:6000/api/v1/forecast?iri_to_forecast=https%3A%2F%2Fwww.theworldavatar.com%2Fkg%2Fontopowsys%2FBusNode_6_AbsorbedReactivePower&interval=http%3A%2F%2Fexample.org%2FOptimisationInterval_1"

```

Note that the URL part is enclosed in quotation marks and the special characters are URL-encoded to ensure that the request is correctly interpreted. Otherwise, it would look like this:

```
curl -X GET http://localhost:6000/api/v1/forecast?iri_to_forecast=https://www.theworldavatar.com/kg/ontopowsys/BusNode_6_AbsorbedReactivePower&interval=http://example.org/OptimisationInterval_1
```