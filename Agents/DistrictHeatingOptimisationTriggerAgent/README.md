# District Heating Optimisation Trigger Agent

This `District Heating Optimisation Trigger Agent` can be used to trigger recurring optimisations of the district heating system in Pirmasens (including the simulation of corresponding emission dispersion) according to the latest use case design using [chained derivations]. For each time step to simulate (i.e., using Aermod Agent), a corresponding `SimulationTime` instance is instantiated or updated, together with a time `Interval` instance representing the associated optimisation horizon (for the DH Optimisation Agent) and two time `Duration`s specifying the length of historical time series data required for the Forecasting Agent. For details, please see the provided link to the derivation design together with the [OntoTimeSeries (Miro board)] and [OntoHeatNet (Miro board)].

On agent startup, the specified SPARQL endpoint will also be populated with all triples provided as `.ttl` files in the [resources/triples] folder, e.g., upload required descriptions of used forecasting models and static point source instances.


# 1. Setup

The dockerised agent can be deployed as standalone version (i.e., outside a larger Docker stack) or deployed to an (existing) stack. 

## 1.1. Prerequisites

Before starting the agent, the `disp:hasOntoCityGMLCityObject` range instances in the [static_point_sources.ttl] file need to be populated manually with the corresponding exhaust outlets/chimneys, as there is currently no way to extract these CityObject IRIs programmatically. The agent will not start in case syntactically invalid IRIs are provided. As the entire [resources] folder is mounted into the container, no rebuilding is required after changing the triples to upload; a simple restart shall be sufficient.

The published agent image assumes the stack name to be `dhstack`. This is because this agent is an integral part of a larger stack and requires another service to be finished before it can start up. To determine when this is the case that service is curled via `dhstack-dh-instantiation`. Further details can be found [here](https://github.com/cambridge-cares/pirmasens/tree/main/districtheating_stack).<br>
To deploy this agent to another stack, please adjust the stack name in the [delayed startup script] prior to re-building the image.


## 1.2. Stand-alone Deployment

Several key environment variables need to be set in the [docker compose file]:

```bash
# Required environment variables for "standalone deployment"
- QUERY_ENDPOINT=       # SPARQL query endpoint
- UPDATE_ENDPOINT=      # SPARQL update endpoint
- STACK_NAME=           # to be left blank!
# Derivation Agent service IRIs required to establish derivation markups
- FORECASTING_AGENT_IRI=
- DH_OPTIMISATION_AGENT_IRI=
- EMISSION_ESTIMATION_AGENT_IRI=
# Dispersion Interactor HTTP URL to establish dispersion derivation markup (via POST request)
- DISPERSION_INTERACTOR_URL=
```

The `STACK_NAME` variable is used to identify the deployment mode of the agent (and a missing one will result in an error). In case the `STACK_NAME` is left blank, Blazegraph endpoint settings will be taken from the docker-compose file. Otherwise they will be retrieved using the StackClients.

To build and publish the agent Docker image please use the following commands. Please note that all of those commands are bundled in the  `publish_docker_image.sh` convenience script.

```bash
# Building the Docker image
docker compose -f docker-compose.yml build
# Publish the Docker image to the Github container registry
docker image push ghcr.io/cambridge-cares/<image tag>:<version>
```

Deploy the agent image with the following command in the same location where this README is located:

```bash
# Deploy the Docker image locally
docker compose -f docker-compose.yml up
```

## 1.3. Stack Deployment

Several key environment variables need to be set in the [stack-manager-input-config file]:

```bash
    "Env": [
        # Target Blazegraph namespace
        "NAMESPACE=kb"
        # Derivation Agent service IRIs required to establish derivation markups
        "FORECASTING_AGENT_IRI=<agent service iri>",
        "DH_OPTIMISATION_AGENT_IRI=<agent service iri>",
        "EMISSION_ESTIMATION_AGENT_IRI=<agent service iri>"
        # Dispersion Interactor HTTP URL to establish dispersion derivation markup
        "DISPERSION_INTERACTOR_URL=<agent http url>"
    ],
```

**Please note:** The specified namespace needs to exist/be created in Blazegraph beforehand to avoid agent execution issues.

If you want to spin up this agent as part of a stack, do the following:
1) Pull the Docker image `docker pull ghcr.io/cambridge-cares/dh-optimisation-trigger-agent:1.1.1` (alternatively, build with the commands above, but do not spin up the image)
2) Adjust both the path of the bind mount and stack-internal URLs in the [stack-manager-input-config file], as the entire [resources] folder is mounted into the agent container
3) Copy the `dh-optimisation-trigger-agent.json` file from the [stack-manager-input-config] folder into the `inputs/config/services` folder of the stack manager
4) Add the service to a corresponding stack configuration json in `inputs/config`
5) Start the stack manager as usual (i.e. `bash ./stack.sh start <STACK_NAME>` from the stack-manager repo). This should start the container. Please use a bash terminal to avoid potential issues with inconsistent path separators.

For more details see the stack manager README on [specifying custom containers].

## 2. Using the Agent

The agent accepts POST requests with json bodies and requires the following keys. The agent locks after receiving a request and unlocks after the optimisation is finished. The agent will not accept any further requests while locked and provide a HTTP 423 status code instead.

- start: (string) Optimisation start dateTime in UTC, indicating the first time step to optimize and forecast
- optHorizon: (integer) Length of the optimisation horizon in time steps (i.e., number of time steps to be forecast and optimised in each iteration)
- numberOfTimeSteps: (integer) Number of total time steps to optimise
- timeDelta: (string) Spacing of optimisation time steps. Possible values are "day", "hour", "minute", or "second".
- heatDemandDataLength: (integer) Length (i.e., number of time steps with given time delta) of historical heat demand time series to use for forecasting
- gridTemperatureDataLength: (integer) Length of historical grid temperature time series to use for forecasting

An example HTTP request is provided in the [example_opt_request] file.

## Debug the agent within the stack

1) Overwrite `Command` in stack-manager config file with (i.e., to keep container alive indefinitely while doing nothing)
    ```
    "ContainerSpec": {
        "Command": [
            "tail", "-f",  "/dev/null"
        ],
        "Env": ...
    ```
2) Right click on running agent container -> select "Attach Visual Studio Code"
3) Install required VSCode extensions inside the container (Python, REST client)
4) Please comment/uncomment the two places in the [flaskapp init] file to avoid asynchronous deployment via Celery (highlighted with a `NOTE`)
5) Start redis server and Celery by running `redis-server & celery -A agent.celery worker --loglevel=info` in terminal (to avoid startup issues)
6) Start local debugging session by running [flaskapp init] in debug mode
7) Sent `resources/debug_request.http` locally from within the agent container


&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), July 2023

<!-- Links -->
<!-- websites -->
[OntoTimeSeries (Miro board)]: https://miro.com/app/board/uXjVPFaO5As=/
[OntoHeatNet (Miro board)]: https://miro.com/app/board/uXjVOhnB9_4=/
[chained derivations]: https://lucid.app/publicSegments/view/a00b553e-d9d1-4845-97b7-f480e980898e/image.png
[specifying custom containers]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#specifying-custom-containers

<!-- files -->
[flaskapp init]: ./agent/flaskapp/__init__.py
[docker compose file]: ./docker-compose.yml
[stack-manager-input-config]: ./stack-manager-input-config
[stack-manager-input-config file]: ./stack-manager-input-config/dh-optimisation-trigger-agent.json
[resources]: ./resources
[resources/triples]: ./resources/triples/
[example_opt_request]: ./resources/example_opt_request.http
[static_point_sources.ttl]: ./resources/triples/static_point_sources.ttl
[delayed startup script]: ./delayed_start.sh