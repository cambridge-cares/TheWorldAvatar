# Time Series handling within The World Avatar project

All time series data within The World Avatar should be stored using the utilities provided by this library. The two main classes are the `TimeSeries` and the `TimeSeriesClient`. The `TimeSeries` class is used to represent and (temporarily) store time series data. The `TimeSeriesClient` represents the **main** class to store and retrieve time series data in/from the KG. To avoid representing every single time series data point semantically in the KG, any time series will be semantically marked up as such in the KG, while the actual data gets stored in an associated RDB (e.g. PostgreSQL).

Key properties of **`TimeSeries`** class:
-- Store time series data (temporarily during program execution)
-- Is returned as result from `TimeSeriesClient`
-- Is argument to `TimeSeriesClient` for adding data

Key properties of **`TimeSeriesClient`** class:
-- Interact with KG and RDB to (permanently) store or retrieve time series data
-- Relies on `TimeSeriesSPARQL` to interact with KG
-- Relies on `TimeSeriesRDBClient` to interact with RDB

Both classes support a time series to consist of several individual (but related) measures, which share the same time column. Any generic Java types (e.g. date time, integer, double etc.) are supported and a robust mapping between any dataIRI (stored in KG) and the respective values (stored in RDB) is ensured. 
Initialising any new time series using the `TimeSeriesClient` creates all required relationships in the KG as well as corresponding table(s) in the RDB. Adding or deleting time series data only adds or deletes data points in the RDB, respectively. Deleting time series via the `TimeSeriesClient` deletes all associated relationships from the KG as well as corresponding table(s) in the RDB. Detailed descriptions of particular functions are provided in the respective docstrings. 


## Time series instantiation
The namespaces used in this document (`ts` denotes the time series ontology and `kb` refers to the namespace to which the time series shall be added):
```
ts : https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#
kb : http://www.theworldavatar.com/kb/ontotimeseries/
```

#### Instantiation in KG ####
Upon instantiation of a time series for any `<entity>` in the KG, the following triples will be created:
```
<entity>  ts:hasTimeSeries  kb:TimeSeries_UUID
kb:TimeSeries_UUID  ts:hasRDB  <Postgres URL>
kb:TimeSeries_UUID  ts:hasTimeUnit  <timeUnit>
```
The created `UUID` denotes a UUID version 4. The data properties `Postgres URL` and `timeUnit` are Literals describing the link to the RDB and the time format in which the data is stored, respectively.

#### Instantiation in RDB ####
In PostgreSQL, the table and column names are restricted to 63 characters. Hence, a central **lookup table** is required to map any `dataIRI` to its corresponding location, i.e. `tableName` and `columnName`.

During initialisation, each `dataIRI` is assigned a `tableName` and `columnName` to make this as robust as possible. Table names will be generated using unique identifiers (i.e. UUIDs), while column names are represented as "column <enumerator>", e.g. column 1, column 2, etc. For better reference, the `timeseriesIRI` associated with the `dataIRI` is also stored in the central table.

During initialisation of the individual time series tables (e.g. Table 1, Table 2 below), the corresponding data type for each dataIRI (i.e. column) needs to be explicitly provided, e.g. Double, Integer, etc. The data type for the time column needs to be specified when initialising the `TimeSeriesClient` itself and does not need to be provided explicitly again.

A schematic depiction for two time series, `time series 1` associated with two dataIRIs (`measure 1` and `measure 2`) and `time series 2` associated with three dataIRIs (`measure 3`, `measure 4` and `measure 5`) is given below: 

**Central RDB lookup table**
| dataIRI | timeseriesIRI | tableName | columnName |
| :---: | :---: | :---: | :---: |
| measure 1 | time series 1 | table 1 | column 1 |
| measure 2 | time series 1 | table 1 | column 2 |
| measure 3 | time series 2 | table 2 | column 1 |
| measure 4 | time series 2 | table 2 | column 2 |
| measure 5 | time series 2 | table 2 | column 3 |

**Table 1**
This table contains all information for `time series 1`: `column 1` contains all time series values for `measure 1`, while `column 2` contains all values for `measure 2`, both sharing the same time entries in column `time`.
| time | column 1 | column 2 |
| :--: | :--: | :--: |
| t1 | ... | ... |
| t2 | ... | ... |

**Table 2**
This table contains all information for `time series 2`.
| time | column 1 | column 2 | column 3 |
| :--: | :--: | :--: | :--: |
| t1 | ... | ... | ... |
| t2 | ... | ... | ... |

## Examples on how to use the TimeSeriesClient
- **Integration tests**:
Detailed integration tests for the `TimeSeriesClient` as well as the (underlying) `TimeSeriesRDBClient` and `TimeSeriesSparql` are provided in the respective [test repository]. Please note that all integration tests use the Testcontainers Java library and, hence, require Docker to be installed.
- **Agent examples**:
Several agents provide working examples of how to use the TimeSeriesClient, e.g.
-- [AQMeshInputAgent] queries and stores time series data from AQ Mesh sensor in Singapore
-- [FloodAgent] queries water level data from the Environment Agency, stores it in the KG, and retrieves it for visualisation
-- [GasGridAgent] queries instantaneous gas flow data from the National Grid, stores it in the KG, and retrieves it for visualisation (via py4jps)

[//]: # (These are reference links used in the body)

   [test repository]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/test/java/uk/ac/cam/cares/jps/base/timeseries>
   [AQMeshInputAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/AQMeshInputAgent>
   [FloodAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/136-dev-floodsensors/Agents/FloodAgent>
   [GasGridAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/1161-dev-gas-grid-input-agent/Agents/GasGridAgent/src/gasgridagent>

