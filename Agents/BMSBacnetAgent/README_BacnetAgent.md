##BMS Bacnet Agent
This agent is an obselete version of periodically writing Data from a Bacnet network to KG as Timeseries. Reading and writing TimeSeries from/into the KG relies on the [TimeSeriesClient]. Refer to [BMSQueryAgent] which is an up-to-date agent of similar functionality.



##Usage
###1. Setup
A csv file contains mappings from the bacnet id to TWA Timeseries IRI is to be put in the working directory. An example of such csv:
```commandline
http://test.com/Timeseries_uuid1, 784
http://test.com/Timeseries_uuid2, 785
...
```
The path of this file should be defined in the [config file] as well as a few other configuratiion parameters.
```
# The mapping csv path
BMS_DEVICE_FILE = 

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

[config file]: ./config/config.py

[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries

[BMSQueryAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BMSQueryAgent