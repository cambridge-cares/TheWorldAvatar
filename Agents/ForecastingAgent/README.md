# Forecasting Agent

This `Forecasting Agent` can be used to read time series data from a knowledge graph (KG), create a time series forecast, and instantiate the forecast back into the KG using the [OntoTimeSeries] ontology. Reading and writing time series from/into the KG relies on the [TimeSeriesClient] and forecasts are created using either pre-trained models or Facebook's [Prophet] (default). 

The Python library [Darts] is used to define, train, and store forecasting models as well as to create the forecasts. It contains a variety of models, from classics such as ARIMA to deep neural networks. 

The agent relies on [py4jps] to access the [JPS_BASE_LIB] (incl. the [TimeSeriesClient]) and is implemented as Flask App to respond to HTTP requests.

```diff
- The agent is currently not Dockerised; however, this is planned for the near future.

- add section about ghcr.io
```
```

# 1. Setup

This section specifies the minimum requirements to run the agent. 

&nbsp;
## 1.1 Prerequisites


### **1) Setting up a virtual environment setup**

It is highly recommended to use a virtual environment for this project. The virtual environment can be created as follows (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variables):

`(Windows)`
```cmd
$ python -m venv forecasting_venv
$ forecasting_venv\Scripts\activate.bat
(forecasting_venv) $
```
The above commands will create and activate the virtual environment `forecasting_venv` in the current directory.

### **2) Installation of required packages**

Install the `forecasting` project including all required packages listed in `setup.py`:

`(Windows)`
```
python -m pip install --upgrade pip  
python -m pip install -e . 

```
Or  to enable running tests:

`(Windows)`
```
python -m pip install -e .[dev]
```
If you intend to use a forecasting model pre-trained with [Darts]: Please be aware of potential issues when loading the model, in case of version clashes between the current environment and the one used for training.

### **3) Instantiated knowledge graph with time series**

In order to forecast a time series, it needs to be instantiated using the [TimeSeriesClient] beforehand. 
[The district heating instantiation module](https://github.com/cambridge-cares/pirmasens) provides an example of how that can look like. In case you do not have access you might reach out to _sm453\<at>cam.ac.uk_ to get access.

### **4) Endpoints**

Set your `PostgreSQL` database and `Blazegraph` endpoints in your [properties file]. 


&nbsp;
# 2. Using the Agent
## General workflow


This section describes the workflow and most important steps to access and extent the agent.
The Agent UML diagram provides an overview of how the agent works:

<p align="center">
    <img src="https://lucid.app/publicSegments/view/2f775ad5-4445-4036-8965-0021df53f6d9/image.png" alt="drawing" width="500"/>
</p>

The `Forecasting Agent` forecasts an existing time series in an KG using its `iri`. After verifying the received HTTP request, the agent loads a model configuration from the [mapping file]. This is either the `DEFAULT` one (which will use [Prophet]) or else must be specified with the `use_model_configuration` parameter in the HTTP request to use a pre-trained model other than Prophet. The [mapping File ] describes in detail what a model configuration `dict` consists of. 

Next the agent loads the time series (+ `covariates` if `load_covariates_func` is given in the loaded configuration) with the TSClient. 

Then, it loads the model. This is either a pre-trained model specified in the model configuration with the model link `model_path_pth_link` and the checkpoint link `model_path_ckpt_link` or else a new Prophet model is fitted to predict the data. The forecast starts from the optional parameter `forecast start date` in the request or if not specifed the last available date is taken. The forecast lasts over the number of specified time steps (`horizon`).

Finally the forecasted time series is instantiated. For that purpose a new `forecast iri` is created and attached to the `iri` specified in the request. Further metadata, e.g. which data and models are used, are included as well using the [OntoTimeSeries] ontology.


## Starting the agent
Buy running  
```
python forecasting\flaskapp\wsgi.py
```
or [main in wsgi.py](./forecasting/flaskapp/wsgi.py) the Flask App with the agent starts. To check if the agent started up properly, open the URL address the agent is running on, e.g. `http://127.0.0.1:5000` in your browser. 


&nbsp;
## Send HTTP requests

Forecasting a time series is triggered by received HTTP requests. An example request to forecast an `iri` is provided in [HTTP_Request_forecast]: 

### Input parameters
- **iri**: the `iri` of the instance which has a time series attached to it. This iri will receive the `hasForecastedValue` relationship.
- **horizon**: the number of time steps the agent forecasts autorecursively into the future.
- **forecast_start_date**: the start `dateTime` of the forecast. If not specified, simply the last value is taken as a starting point. The series is split at this point and future available data is used to calculate the forecasting error.
- **data_length**: the number of values loaded before `forecast_start_date`. This data is used directly as input to fit [Prophet] or to scale the input for the pre-trained neural network. (If not set the default value from the [mapping file] is used)
- **use_model_configuration**: if specified this model configuration from the [mapping file] is used.  


## Custom model configurations and new models
Specify your custom configurations following the example of the `TFT_HEAT_SUPPLY` model configuration in the [mapping file]. 

If you need covariates, define a function which load them (similarly to `get_covs_heat_supply`) for the `load_covariates_func` parameter in your configuration. To use your own pre-trained model with [Darts], expand the [agent module] where `load_pretrained_model` is called just like the model for `TFT_HEAT_SUPPLY` is loaded. You can use the function `load_pretrained_model` as well if thats suits your model, just specify your model class and set the `input_length` as for `TFT_HEAT_SUPPLY`. 


# 3. How to run tests
 <span style="color:red"> Be aware: The test will clear your Blazegraph namespace!! </span> Therefore, you should create a new Blazegraph test namespace. Follow those steps:

1. Create and activate a virtual environment and install all required packages as described under [Setup](#1-setup).

2. A [docker-compose.test.yml] file is provided to spin up a stack with a Blazegraph and a PostgreSQL container. No volumes are used as data is only used for testing.
```bash
# Spin up container stack
docker-compose -f "docker-compose.test.yml" up -d
```
3.  Set your postgres database and blazegraph endpoints in your properties [properties file] by simply uncommenting the test endpoints (and commenting out the production endpoints). As testing uses the default endpoints (`kb` for Blazegraph and `postgres` for PostgreSQL) they do not need to be created manually beforehand.

4. To start all tests run in your console:
```
pytest tests/
```

&nbsp;
# Authors #
Magnus Mueller (mm2692@cam.ac.uk), November 2022

Markus Hofmeister (mh807@cam.ac.uk), November 2022


<!-- Links -->
<!-- websites -->
[py4jps]: https://pypi.org/project/py4jps/#description
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[Darts]: https://unit8co.github.io/darts/index.html
[Prophet]: https://github.com/facebook/prophet

<!-- files -->
[properties file]: ./resources/timeseries.properties
[HTTP_Request_forecast]: ./resources/HTTP_request_forecast.http
[agent module]: /forecasting/forecasting_agent/agent.py
[mapping file]: /forecasting/datamodel/data_mapping.py
[docker-compose.test.yml]: ./docker-compose.test.yml
