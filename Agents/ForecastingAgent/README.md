# Forecasting Agent

The "Forecasting Agent" forecasts an existing time series in an RDB using its dataIRI. The agent re-instantiates the forecasted time series under the same dataIRI. Different pre-trained models can be used, and if none matches the dataIRI, a new Prophet model is fitted to predict the new data.


<span style="color:red">Tests are currently still excluded.</span>

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
If you use later a model pretrained with 'darts', conflicts can occur while loading the model, if your version differs from the version with which the model was trained.

### **3) Instantiated knowledge graph with time series**

In order to forecast a time series, this series has to be instantiated in a RDB. It is necessary that the ontology of the time series instantiation equals the ontology provided by [the time series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries).    

### **4) Endpoints**

Set your postgres database and blazegraph endpoints in your properties [file](./resources/timeseries.properties). 


&nbsp;
# 2. Using the Agent

Buy running [main](./forecasting/flaskapp/wsgi.py) the flask app starts.  


&nbsp;
## Send http requests
[HTTPRequest_forecast](./resources/HTTP_request_forecast.http) shows a sample request to forecast a dataIRI. 

### Input parameters
- **dataIRI** is the IRI of the existing TS, which should receive the hasForecastedValue instantiation.
- **horizon** the time steps the agent forecasts autorecursively into the future.
- **forecast_start_date** is the start day of the forecast, if not specified, simple the last value is taken as a starting point. The series is split here and future available data is used to calculate the forecasting error.
- **data_length** determines the number of values loaded before `forecast_start_date`. This data is used directly as input to fit prophet or to scale the input for the pre-trained neural network.
If not set the default value from the [mapping file] is used.
- **force_mapping** if specified this configuration from the [mapping file] is used. Otherwise the agent identifies the `dataIRI`.   


## Custom configurations and new models
Specify your custom configurations following the example of the `TFT_HEAT_SUPPLY` configuration in the [mapping file]. 

To identify a specific `dataIRI` and map it to your configuration, edit the `get_config` function in the [mapping file]. First retrieve properties like the rdf type or label of your `dataIRI` to identify it uniquely. Next compare the properties with your required properties following the example in `get_config`. Finally return if match the dictionary key of our configuration of the MAPPING dictionary from the [mapping file].  




&nbsp;
# Authors #
Magnus Mueller (mm2692@cam.ac.uk), November 2022
Markus Hofmeister (mh807@cam.ac.uk), October 2022


<!-- Links -->
<!-- websites -->
[agent file]: /forecasting/forecasting_agent/agent.py
[mapping file]: /forecasting/datamodel/data_mapping.py
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Container registry on Github]: https://ghcr.io
[Github package repository]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages
[http://localhost:5000/]: http://localhost:5000/
[Java Runtime Environment version >=11]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[JDBC driver]: https://jdbc.postgresql.org/download/ 
[OntoBuiltEnv]: http://www.theworldavatar.com/ontology/ontobuiltenv/OntoBuiltEnv.owl
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[py4jps]: https://pypi.org/project/py4jps/#description
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[HM Land Registry Open Data]: https://landregistry.data.gov.uk/
[Price Paid Linked Data]: https://landregistry.data.gov.uk/app/root/doc/ppd
[UK House Price Index Linked Data]: https://landregistry.data.gov.uk/app/ukhpi/doc
[HM Land Registry SPARQL endpoint]: http://landregistry.data.gov.uk/landregistry/query

<!-- github -->
[Common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[credentials]: https://github.com/cambridge-cares/TheWorldAvatar/tree/1376-dev-building-matching-agent/Agents/BuildingMatchingAgent/credentials
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[EPC Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-EPCInstantiationAgent/Agents/EnergyPerformanceCertificateAgent

<!-- files -->
[Dockerfile]: ./Dockerfile
[docker compose file]: ./docker-compose.yml
[resources]: ./resources
[stack.sh]: ./stack.sh
[stack_configs]: ./landregistry/utils/stack_configs.py