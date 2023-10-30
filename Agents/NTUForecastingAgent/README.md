# NTU Forecasting Agent

This agent is designed to deploy the forecasting agent for the NTU energy scenario. In order to produce forecasts, requests can be sent to this agent which, if necessary, instantiates triples in the knowledge graph and finally lets the forecasting agent predict the time series according to the request's specified arguments. Thus, the NTU forecasting agent can only be deployed in a stack together with the forecasting agent.

# 1. Setup

Setup the forecasting agent as part of a stack following the instructions of the [forecasting agent](https://github.com/cambridge-cares/TheWorldAvatar/edit/main/Agents/ForecastingAgent/).

In order to fill the knowledge graph with the data of the NTU scenario, the instructions for adding the [Historical NTUEnergy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) to the stack should be executed as well.


#### Build the agent

Build the image of the agent locally:
```
docker build -t ntu_forecasting_agent:1.0.0 .
```

#### Add config to the Stack Manager

Copy ```./NTUForecastingAgent/stack_manager_config/ntu-forecasting-agent.json``` to ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/```, where the  configs of the forecasting agent and historical NTUEnergy agent should be as well.

If the instructions for the forecasting agent and c were followed correctly, the file ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json``` should exist. Add the NTU forecasting Agent in this file in the same fashion as for the other agents. it should look like this:

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

#### setup correct namespace

The default namespace of the forecasting agent in ```forecasting-agent.json``` in specified as ```kb```, which is not the namespace the forecasting-agent.json uses. Therefore, change ```NAMESPACE``` in ```TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/forecasting-agent.json``` to ```ntuenergy```. It should look like this:

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