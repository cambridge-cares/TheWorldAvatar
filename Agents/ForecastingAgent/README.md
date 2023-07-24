# Forecasting Agent

This `Forecasting Agent` can be used to read time series data from The World Avatar (TWA), create a time series forecast, and instantiate the forecast back into the knowledge graph (KG) using the [OntoTimeSeries] ontology. Reading and writing time series from/into the KG relies on the [TimeSeriesClient] and forecasts are created using either pre-trained models or Facebook's [Prophet] (default). 

The Python library [Darts] is used to define, train, and store forecasting models as well as to create the forecasts. It contains a variety of models, from classics such as ARIMA to deep neural networks. 

The agent relies on [py4jps] to access the [JPS_BASE_LIB] (incl. the [TimeSeriesClient]) and is implemented as Flask App to respond to HTTP requests.

&nbsp;
# 1. Setup

This section specifies the minimum requirements to build and deploy the Docker image. 

&nbsp;
## 1.1 Prerequisites

The dockerised agent can be deployed as standalone version (i.e. outside a larger Docker stack) or deployed to an (existing) stack. Several key environment variables need to be set in the [Docker compose file]:

```bash
# Required environment variables for both Stack and "standalone" (i.e. outside stack) deployment
- STACK_NAME=TEST         # to be left blank for "standalone" deployment
# Additional environment variables required for Stack deployment
# (can be left blank for "standalone" deployment)
- NAMESPACE=test          # Target Blazegraph namespace
- DATABASE=test           # Target PostGIS/PostgreSQL database name
# Additional environment variables required for "standalone deployment"
# (can be left blank for Stack deployment)
- DB_URL=
- DB_USER=
- DB_PASSWORD=
- SPARQL_QUERY_ENDPOINT=
- SPARQL_UPDATE_ENDPOINT=
```

The `STACK_NAME` variable is used to identify the deployment mode of the agent. In case the `STACK_NAME` is left blank, default Postgres and Blazegraph endpoint setting will be taken from the docker-compose file. Otherwise they will be retrieved using the StackClients based on the provided `NAMESPACE` and `DATABASE` variables.

**Please note**: 
1) All variables defined here (except for `STACK_NAME`) serve as default values. To omit any of those default values, either remove the key completely or just leave it blank. If corresponding keys are provided in the HTTP request to the agent, those will be used instead. Otherwise, these default values will be used. In case neither is provided, the agent will cause an exception.

2) A missing `STACK_NAME` variable will result in an error; however, when deploying using the stack-manager start up script, the `STACK_NAME` variable will be set automatically for all services. Hence, this could be left blank here; however, if provided, it needs to match the `STACK_NAME` used by the stack-manager!

&nbsp;
## 1.2 Miscellaneous

**Ensure access to CMCL Docker registry**:
The required `stack-clients-*.jar` resource to be added to py4jps during building the Docker image is retrieved from the Stack-Clients docker image published on `docker.cmclinnovations.com`. Hence, access to the CMCL Docker registry is required from the machine building the agent image. For more information regarding the registry, see the [CMCL Docker registry wiki page].

**Ensure access to Github container registry**:
A `publish_docker_image.sh` convenience script is provided to build and publish the agent image to the [Github container registry]. To publish a new image, your github user name and [personal access token] (which must have a `scope` that [allows you to publish and install packages]) needs to be provided. 

In order to avoid potential launching/debugging issues using the provided `tasks.json` shell commands, please ensure the `augustocdias.tasks-shell-input` plugin is installed.

If you intend to use a forecasting model pre-trained with [Darts]: Please be aware of potential issues when loading the model, in case of version clashes between the current environment and the one used for training.

&nbsp;
# 2. Using the Agent
## 2.1 General workflow


This section describes the workflow and most important steps to access and extent the agent.
The Agent UML diagram provides an overview of how the agent works:

<p align="center">
    <img src="https://lucid.app/publicSegments/view/2f775ad5-4445-4036-8965-0021df53f6d9/image.png" alt="drawing" width="500"/>
</p>

The `Forecasting Agent` forecasts an existing time series in an KG using its `iri`. After verifying the received HTTP request, the agent loads a model configuration from the [mapping file]. This is either the `DEFAULT` one (which will use [Prophet]) or else must be specified with the `use_model_configuration` parameter in the HTTP request to use a pre-trained model other than Prophet. The [mapping File ] describes in detail what a model configuration `dict` consists of. 

Next the agent loads the time series (+ `covariates` if `load_covariates_func` is given in the loaded configuration) with the TSClient. 

Then, it loads the model. This is either a pre-trained model specified in the model configuration with the model link `model_path_pth_link` and the checkpoint link `model_path_ckpt_link` or else a new Prophet model is fitted to predict the data. The forecast starts from the optional parameter `forecast start date` in the request or if not specifed the last available date is taken. The forecast lasts over the number of specified time steps (`horizon`).

Finally the forecasted time series is instantiated. For that purpose a new `forecast iri` is created and attached to the `iri` specified in the request. Further metadata, e.g. which data and models are used, are included as well using the [OntoTimeSeries] ontology.

&nbsp;
## 2.2 Build and Deploy the Agent

To build and publish the agent Docker image please use the following commands. Please note that all of those commands are bundled in the  `publish_docker_image.sh` convenience script (only target image (i.e. production/debug) needs to be adjusted at top of that script).

```bash
# Building the Docker image (production / debug)
docker-compose -f docker-compose.yml  build
docker-compose -f docker-compose.debug.yml  build

# Publish the Docker image to the Github container registry
docker image push ghcr.io/cambridge-cares/<image tag>:<version>
```

Time out issues have been observed when building the image. If this happens, please try pulling the required stack-clients image first by `docker pull docker.cmclinnovations.com/stack-client:1.6.2`.

###  **Docker Deployment**

Deploy the dockerised agent by running the following code in the command prompt from the same location where this README is located (ideally, use a bash terminal to avoid potential issues with inconsistent path separators). 

```bash
# Deploy the Docker images (production / debug) locally
docker-compose -f docker-compose.yml  up
docker-compose -f docker-compose.debug.yml  up
```

To verify the correct startup of the agent, open the URL address the agent is running on, e.g. `http://127.0.0.1:5001` in your browser. 

### **Stack Deployment**

If you want to spin up this agent as part of a stack, do the following:
1) Build the production image using the commands provided above (do not spin up the image)
2) Copy the `forecasting-agent.json` file from the [stack-manager-input-config] folder into the `inputs/config` folder of the stack manager
3) Start the stack manager as usual (i.e. `bash ./stack.sh start <STACK_NAME>` from the stack-manager repo). This should start the container. Please use a bash terminal to avoid potential issues with inconsistent path separators.
4) The agent shall become available at `http://<HOST>:<PORT>/forecastingAgent/`

### **Stack Troubleshooting**

In case NGINX faces issues to locate the agent after startup, try the following commands from within the NGINX container:

```bash
# Test NGINX configuration to determine potentially erroneous (upstream) configuration
nginx -t
# Delete potentially erroneous/outdated configuration files
rm /etc/nginx/conf.d/...
rm /etc/nginx/conf.d/locations/...
# Verify that NGINX configuration is corrected
nginx -t
# expected output: the configuration file /etc/nginx/nginx.conf syntax is ok
# expected output: configuration file /etc/nginx/nginx.conf test is successful

# Reload NGINX configuration
nginx -s reload  
# expected output: signal process started
```

## 2.3 Notes on Debugging

The stack deployment of the agent is focused on the production image of the agent. To debug the agent, it is easiest to deploy the debug version locally by providing all required parameters in the [docker-compose.debug.yml] and running the below commands (`build` as required): 
```bash
docker-compose -f docker-compose.debug.yml  build
docker-compose -f docker-compose.debug.yml  up
```
This spins up the agent on `http://127.0.0.1:5001`, waiting for the debugger to attach. To attach to the container and start debugging, please use the provided  `Python: Debug Flask within Docker` debug configuration. Although outside the stack, this procedure allows to debug all essential functionality of the agent (without the need to have a full stack running).


&nbsp;
## 2.4 Forecasting time series via HTTP requests

Forecasting a time series is triggered by receiving an HTTP `POST` request with a JSON body. An example request to forecast an `iri` is provided in [HTTP_Request_forecast]: 

### HTTP request parameters

- **iri**: the `iri` of the instance which has a time series attached to it. This `iri` will receive the `hasForecastedValue` relationship
- **horizon**: the number of time steps to forecast autorecursively into the future
- **forecast_start_date**: the start `dateTime` of the forecast. If not specified, simply the last value is taken as a starting point. The series is split at this point and future available data is used to calculate the forecasting error
- **data_length**: the number of values loaded before `forecast_start_date`. This data is used directly as input to fit [Prophet] or to scale the input for the pre-trained neural network (If not set the default value from the [mapping file] is used)
- **use_model_configuration**: if specified this model configuration from the [mapping file] is used

Further optional parameters can be provided to specify the connection configurations to use. All specified parameters overwrite potential default values specified in the docker-compose file. To use the default values, simply exclude the respective parameter from the HTTP request:
- **namespace**: target Blazegraph namespace (only relevant for Stack deployment)
- **database**: target PostGIS database (only relevant for Stack deployment)
- **query_endpoint**: SPARQL query endpoint (only relevant for standalone deployment)
- **update_endpoint**: SPARQL update endpoint (only relevant for standalone deployment)
- **db_url**: PostGIS/PostgreSQL database URL (only relevant for standalone deployment)
- **db_user**: PostGIS/PostgreSQL database user (only relevant for standalone deployment)
- **db_password**" PostGIS/PostgreSQL database password (only relevant for standalone deployment)


## 2.5 Custom model configurations and new models
Specify your custom configurations following the example of the `TFT_HEAT_SUPPLY` model configuration in the [mapping file]. 

If you need covariates, define a function which load them (similarly to `get_covs_heat_supply`) for the `load_covariates_func` parameter in your configuration. To use your own pre-trained model with [Darts], expand the [agent module] where `load_pretrained_model` is called just like the model for `TFT_HEAT_SUPPLY` is loaded. You can use the function `load_pretrained_model` as well if thats suits your model, just specify your model class and set the `input_length` as for `TFT_HEAT_SUPPLY`. 

&nbsp;
# 3. How to run tests

Both unit tests and dockerised integration tests are provided. Testcontainers based on the [docker-compose.test.yml] file are used to spin up a stack with a Blazegraph and a PostgreSQL container for the integration tests. No volumes are used as data is only used for testing.

**Setting up the testing environment**

It is highly recommended to use a virtual environment for testing, which can be created via
`(Windows)`
```cmd
$ python -m venv forecasting_venv
$ forecasting_venv\Scripts\activate.bat
(forecasting_venv) $
```

Install the `forecasting` project including all packages required for testing as listed in `setup.py` via
`(Windows)`
```
(forecasting_venv) $ python -m pip install --upgrade pip  
(forecasting_venv) $ python -m pip install -e .[dev]
```

**Running tests**

You can start all tests by running the following command in your console:
```bash
# Run all tests
pytest --docker-compose=docker-compose.test.yml
# Run all tests with verbose output (i.e. logging output)
pytest -s --docker-compose=docker-compose.test.yml
```


# Final Remarks
Currently the agent holds several (helper) scripts tailored towards the House 45 use case in Pirmasens (inside the resources folder). They are mainly here for reference to provide working examples and might be removed in a later revision of the agent.


&nbsp;
# Authors #
Magnus Mueller (mm2692@cam.ac.uk), November 2022

Markus Hofmeister (mh807@cam.ac.uk), December 2022


<!-- Links -->
<!-- websites -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[CMCL Docker registry wiki page]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[py4jps]: https://pypi.org/project/py4jps/#description
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Darts]: https://unit8co.github.io/darts/index.html
[Prophet]: https://github.com/facebook/prophet
[Github container registry]: https://ghcr.io
[personal access token]: https://docs.github.com/en/github/

<!-- files -->
[properties file]: ./resources/timeseries.properties
[HTTP_Request_forecast]: ./resources/HTTP_request_forecast.http
[agent module]: ./forecasting/forecasting_agent/agent.py
[mapping file]: ./forecasting/datamodel/data_mapping.py
[docker-compose.debug.yml]: ./docker-compose.debug.yml
[docker-compose.test.yml]: ./docker-compose.test.yml
[Docker compose file]: ./docker-compose.yml
[stack-manager-input-config]: ./stack-manager-input-config