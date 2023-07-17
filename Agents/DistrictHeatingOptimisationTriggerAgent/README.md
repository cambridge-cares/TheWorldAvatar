# District Heating Optimisation Trigger Agent

This `District Heating Optimisation Trigger Agent` can be used to trigger recurring optimisations of the district heating system in Pirmasens (including the simulation of corresponding emission dispersion) according to the latest use case design using [chained derivations]. For each time step to optimise/simulate, a corresponding `SimulationTime` instance is instantiated or update, together with a time `Interval` instance representing the current optimisation horizon and two time `Durations` specifying the length of historical time series data required for the Forecasting Agent. 

For details, please see the provided link to the derivation design together with the [OntoTimeSeries (Miro board)] and [OntoHeatNet (Miro board)].


# 1. Setup

The dockerised agent can be deployed as standalone version (i.e., outside a larger Docker stack) or deployed to an (existing) stack. 

## 1.1. Stand-alone Deployment

Several key environment variables need to be set in the [Docker compose file]:

```bash
# Required environment variables for "standalone deployment"
- QUERY_ENDPOINT=       # SPARQL query endpoint
- UPDATE_ENDPOINT=      # SPARQL update endpoint
- STACK_NAME=           # to be left blank!
```

The `STACK_NAME` variable is used to identify the deployment mode of the agent (and a missing one will result in an error). In case the `STACK_NAME` is left blank, Blazegraph endpoint setting will be taken from the docker-compose file. Otherwise they will be retrieved using the StackClients.

To build and publish the agent Docker image please use the following commands. Please note that all of those commands are bundled in the  `publish_docker_image.sh` convenience script.

```bash
# Building the Docker image
docker-compose -f docker-compose.yml  build

# Publish the Docker image to the Github container registry
docker image push ghcr.io/cambridge-cares/<image tag>:<version>
```

Deploy the agent image with the following command in the same location where this README is located:

```bash
# Deploy the Docker image locally
docker-compose -f docker-compose.yml up
```

## 1.2. Stack Deployment

Several key environment variables need to be set in the [stack-manager-input-config file]:

```bash
    "Env": [
        "NAMESPACE=kb"      # Target Blazegraph namespace
    ],
```

**Please note:** The specified namespace needs to exist/be created in Blazegraph beforehand to avoid agent execution issues.

If you want to spin up this agent as part of a stack, do the following:
1) Build the Docker image using the commands provided above (do not spin up the image)
2) Copy the `dhoptimisationtrigger_agent.json` file from the [stack-manager-input-config] folder into the `inputs/config/services` folder of the stack manager
3) Add the service to a corresponding stack configuration json in `inputs/config`
4) Start the stack manager as usual (i.e. `bash ./stack.sh start <STACK_NAME>` from the stack-manager repo). This should start the container. Please use a bash terminal to avoid potential issues with inconsistent path separators.
5) The agent shall become available at `http://<HOST>:<PORT>/dhTriggerAgent/`

For more details see the stack manager README on [specifying custom containers].

## 2. Using the Agent

The agent accepts POST requests with json bodies and requires the following keys. The agent locks after receiving a request and unlocks after the optimisation is finished. The agent will not accept any further requests while locked and provide a HTTP 423 status code instead.

- start: (string) Optimisation start dateTime in UTC, indicating the first time step to optimize and forecast
- mpcHorizon: (integer) Length of the optimisation horizon in time steps
- numberOfTimeSteps: (integer) Number of time steps to optimize
- timeDelta: (string) Spacing of optimisation time steps. Possible values are "day", "hour", "minute", or "second".
- heatDemandDataLength: (integer) Length (i.e., number of time steps with given time delta) of historical heat demand time series to use for forecasting
- gridTemperatureDataLength: (integer) Length of historical grid temperature time series to use for forecasting

An example HTTP request is provided in the [example_mpc_request] file.


&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), July 2023

<!-- Links -->
<!-- websites -->
[OntoTimeSeries (Miro board)]: https://miro.com/app/board/uXjVPFaO5As=/
[OntoHeatNet (Miro board)]: https://miro.com/app/board/uXjVOhnB9_4=/
[chained derivations]: https://lucid.app/documents/view/9fabc350-143b-4ca3-be52-b174c9f82c07
[specifying custom containers]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#specifying-custom-containers

<!-- files -->
[example_mpc_request]: ./resources/example_mpc_request.http
[Docker compose file]: ./docker-compose.yml
[stack-manager-input-config]: ./stack-manager-input-config
[stack-manager-input-config file]: ./stack-manager-input-config/dhoptimisationtrigger_agent.json