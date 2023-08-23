# Emission Agent

...


&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

## 1.1 Agent Settings

...


## 1.2 Miscellaneous

**Only relevant** if you intend to build (and publish) the Docker image:

- Ensure access to CMCL Docker registry: 
    The required `stack-clients-*.jar` resource to be added to [py4jps] during building the Docker image is retrieved from the Stack-Clients docker image published on `docker.cmclinnovations.com`. Hence, access to the CMCL Docker registry is required from the machine building the agent image. For more information regarding the registry, see the [CMCL Docker registry wiki page].

- Ensure access to Github container registry:
    A `publish_docker_image.sh` convenience script is provided to build and publish the agent image to the [Github container registry]. To publish a new image, your github user name and [personal access token] (which must have a `scope` that [allows you to publish and install packages]) needs to be provided. 


## 1.3 Required Derivation Markup

...


&nbsp;
# 2. Agent Operation

...


&nbsp;
# 3. Using the Agent

## 3.1 Building the Agent

To build and publish the agent Docker image, please use the following commands. Please note that both commands are bundled in the  `publish_docker_image.sh` convenience script.

```bash
# Building the (production) image
docker compose -f docker-compose.yml build
# Publish the Docker image to the Github container registry
docker image push ghcr.io/cambridge-cares/<image tag>:<version>
```

Time out issues have been observed when building the image. If this happens, please try pulling the required stack-clients image first by `docker pull docker.cmclinnovations.com/stack-client:1.6.2`.

## 3.2 Deploying the Agent

It is recommended to pull the published Docker image from [Github container registry] for sole deployment (i.e., in case no modifications to the agent are needed):

```bash
# Pull published (production) image
docker pull ghcr.io/cambridge-cares/forecasting_agent:2.0.0
```

###  **Standalone Deployment**

Deploy the dockerised agent by running the following command from the same location where this README is located (ideally, use a bash terminal to avoid potential issues with inconsistent path separators). 

```bash
# Deploy the Docker image locally
docker compose -f docker-compose.yml up
```

To verify the correct startup of the agent, open the URL address the agent is running on, e.g., `http://localhost:5001/` in your browser. 


### **Stack Deployment**

If you want to spin up this agent as part of a stack, do the following:
1) Build OR pull the (production) image using the commands provided above (do not spin up the image)
2) Copy the `emission-agent.json` file from the [stack-manager-input-config] folder into the `inputs/config/services` folder of the stack manager
3) Start the stack manager as usual (i.e. `bash ./stack.sh start <STACK_NAME>` from the stack-manager repo). This should start the container. Please use a bash terminal to avoid potential issues with inconsistent path separators.
4) The agent shall become available at `http://<HOST>:<PORT>/emissionAgent/`


## 3.3 Notes on Debugging

To debug the agent within the stack, follow these steps (a similar appraoch should work for the standalone version)

1) Overwrite command specified in Dockerfile by providing `tail -f /dev/null` `Command` in stack-manager config file (this keeps the container alive indefinitely while doing nothing). An amended `emission-agent_debug` config is provided in the [stack-manager-input-config] folder.
2) Start stack-manager as usual
3) Right click on running agent container -> select "Attach Visual Studio Code"
4) Install required VSCode extensions inside the container
5) Start local debugging session inside container by running `entry_point.py` in debug mode; if HTTP requests from outside do not reach the container, send requests locally from inside the container as workaround


&nbsp;
# 4. Dockerised Agent Tests

Both dockerised unit and integration tests are provided. Tests check for expected behaviour of the forecasting agents with and without overwriting existing forecasts. Hence, 4 containers will be created when running the tests:
- Forecasting Agent, which overwrites existing forecasts when creating new ones
- Forecasting Agent, which does not overwrite existing forecasts (this container also runs pytest)
- Blazegraph and Postgis instances (spun up via Docker in Docker using testcontainers)

```bash
# Build and run dockerised agent tests
docker compose -f "docker-compose-test_dockerised.yml" up -d --build
```

To run the dockerised tests in Debug mode, please run the below script to start up both testing agents and pytest in separate containers (to allow for debugging of the overwriting/non-overwriting forecasting agent versions separately). Subsequently, attach the Debugger(s) using the provided `Python: Debug dockerised tests` and `Python: Debug dockerised agent...` configurations as required (provided in `.vscode` subfolder). Attaching the `Python: Debug dockerised tests` debugger is required to start the tests, while attaching debuggers to both agents under tests are optional:

```bash
# Build and run dockerised agent tests in debug mode
docker compose -f "docker-compose-test_dockerised_debug.yml" up -d --build
```


## Issues
1. Length of the request line exceeds the limit of gunicorn server when `FloodAssessmentAgent` requesting `AverageSquareMetreAgent` and `PropertyValueEstimationAgent` for synchronous derivation update via HTTP GET request, e.g. `Request Line is too large (4807 &gt; 4094)`

    **Workaround:** add `"--limit-request-line", "0"` (unlimited request line length) to the list of `entrypoint` of relevant agents in [docker-compose-agents.yml], see https://docs.gunicorn.org/en/stable/settings.html#limit-request-line **NOTE This should be used with caution especially in production image**
    


&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), August 2023


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Using-Docker-images
[py4jps]: https://pypi.org/project/py4jps/#description
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Darts]: https://unit8co.github.io/darts/index.html
[Prophet]: https://unit8co.github.io/darts/generated_api/darts.models.forecasting.prophet_model.html
[Facebook Prophet]: https://github.com/facebook/prophet
[Github container registry]: https://ghcr.io
[personal access token]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
[Derived Information Framework]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[derivation agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent

[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries
[OntoDerivation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoderivation

<!-- files -->
[HTTP forecast error request]: ./resources/HTTP_evaluate_errors.http
[model mapping]: ./forecastingagent/fcmodels/model_mapping.py
[docker compose file]: ./docker-compose.yml
[stack manager input config file]: ./stack-manager-input-config/forecasting-agent.json
[stack-manager-input-config]: ./stack-manager-input-config
[test_plots]: tests/test_plots/
