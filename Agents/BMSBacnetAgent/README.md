## BMS Bacnet Agent
This agent is an obselete version of periodically writing data from a Bacnet network to the knowledge graph as time series. Reading and writing time series from/into the KG relies on the [TimeSeriesClient]. Refer to [BMSQueryAgent] which is an up-to-date agent of similar functionality.



## Usage
### 1. Configuration
Each reading of a physical laboratory device exists in the Bacnet network with its own unique id. The mappings from each device id to its TimesSeries IRI has to put in the working directory as a csv file. 

An example mapping file of the CARES laboratory Bacnet can be downloaded from this [dropbox] link. Note that this agent is obsolete and no longer maintained, therefore the mapping file is not guaranteed to reflect the current version of the CARES lab Bacnet.


The path of this file should be defined in the [config file] as well as a few other configuration parameters.
```
# The mapping csv path
BMS_DEVICE_FILE = 

# Base IRI of TimeSeries to be updated
DOMAIN=

# BACNET related properties
BACNET_IP = 

# Access information required by TimeSeries Client
PROPERTIES_FILE = 
DB_URL_KEY = 
DB_USER_KEY = 
DB_PASSWORD_KEY = 

# AGENT properties
DB_TABLE = "bms"    # Table name in DB
UPDATE_DURATION =   # time interval in min
```

### 2. Setup

1. Note that this agent needs connections to the Bacnet network, a Blazegraph and a PostgreSQL database. The connections have to be properly configured by providing the correct parameters in the [config file] above. Also, these components are assumed to already be online before this agent starts. 
2. To run in a docker, a Dockerfile is provided.
```commandline
docker build -t bmsbacnet_image .
docker run -d --name bmsbacnet_container bmsbacnet_image
```
3. Else, run locally:
```commandline
pip install -r requirements.txt
python app.py
```
### 3. Expected Output
This agent automatically runs the update job periodically once deployed and does not provide any HTTP APIs. Upon started, the agent should add new timeseries data records to the PostgreSQL database by the update interval (as defined in the [config file]). 

[config file]: ./config/config.py

[TimeSeriesClient]: https://github.com/TheWorldAvatar/baselib/tree/main/src/main/java/uk/ac/cam/cares/jps/base/timeseries

[BMSQueryAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BMSQueryAgent

[dropbox]: https://www.dropbox.com/scl/fi/3wygcubprwmpywm5f5wxx/bmsbacnetmap.csv?rlkey=hwdfcp8cg9ikajw6682n9tg5j&dl=0
