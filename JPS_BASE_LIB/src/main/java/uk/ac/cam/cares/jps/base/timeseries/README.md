# Time Series handling within The World Avatar project

All time series data within The World Avatar should be stored using the utilities provided by this library. The two main classes are the `TimeSeries` and the `TimeSeriesClient`. The `TimeSeries` class is used to represent and (temporarily) store time series data. The `TimeSeriesClient` represents the **main** class to store and retrieve time series data in/from the KG. To avoid representing every single time series data point semantically in the KG, any time series will be semantically marked up as such in the KG, while the actual data gets stored in an associated RDB (e.g. PostgreSQL).

Key properties of the **`TimeSeries`** class:
* Store time series data (temporarily during program execution)
* Is returned as result from `TimeSeriesClient`
* Is argument to `TimeSeriesClient` for adding data

Key properties of the **`TimeSeriesClient`** class:
* Interact with KG and RDB to (permanently) store or retrieve time series data
* Relies on `TimeSeriesSPARQL` to interact with KG
* Relies on `TimeSeriesRDBClient` or `TimeSeriesRDBClientWithReducedTables` to interact with RDB

Both classes support a time series to consist of several individual (but related) measures, which share the same time column. Any generic Java types (e.g. date time, integer, double etc.) are supported and a robust mapping between any dataIRI (stored in KG) and the respective values (stored in RDB) is ensured. 
Initialising any new time series using the `TimeSeriesClient` creates all required relationships in the KG as well as corresponding table(s) in the RDB. Adding or deleting time series data only adds or deletes data points in the RDB. Deleting time series via the `TimeSeriesClient` deletes all associated relationships from the KG as well as corresponding table(s) in the RDB. Detailed descriptions of particular functions are provided in the respective docstrings. 

## Note on PostGIS support
The TimeSeriesClient supports storing and querying geometries in PostGIS by providing objects of org.postgis.Geometry. You must ensure that the PostGIS extension is enabled in PostgreSQL to use this.

## Schema specification
It is possible to store time series data in a separate schema through the method TimeSeriesClient.setRDBSchema(String schema). If this is not set, the TimeSeriesClient defaults to the "public" schema.

## TimeSeriesRDBClientInterface
There are two classes that extend this interface:
1) TimeSeriesRDBClient
    - This is the first and default version.
    - Creates a table for every single time series
    - May not be suitable for applications which require many tables

    Usage example:
    ```java
    TimeSeriesRDBClient<Instant> timeSeriesRdbClient = new TimeSeriesRDBClient<>(Instant.class);
    RemoteStoreClient remoteStoreClient = new RemoteStoreClient(kgUrl, kgUrl);

    TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient(remoteStoreClient, timeSeriesRdbClient);

    RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(rdb, user, password);

    try (Connection conn = rdbStoreClient.getConnection()) {
        timeSeriesClient.initTimeSeries(dataIRIs, dataClass, timeUnit, conn);
        timeSeriesClient.addTimeSeriesData(timeseries, conn);
    }
    ```

2) TimeSeriesRDBClientWithReducedTables
    - May be suitable for applications which require instantiation of new time series repetitively, when using the default option creates too many tables.

    Usage example:
    ```java
    TimeSeriesRDBClientWithReducedTables<Instant> timeSeriesRdbClient = new TimeSeriesRDBClientWithReducedTables<>(Instant.class);
    RemoteStoreClient remoteStoreClient = new RemoteStoreClient(kgUrl, kgUrl);

    TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient(remoteStoreClient, timeSeriesRdbClient);

    RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(rdb, user, password);

    try (Connection conn = rdbStoreClient.getConnection()) {
        timeSeriesClient.initTimeSeries(dataIRIs, dataClass, timeUnit, conn);
        timeSeriesClient.addTimeSeriesData(timeseries, conn);
    }
    ```
* The connection object should be created using Java's try-with-resources block (https://www.baeldung.com/java-try-with-resources) as shown in the example above. This is to ensure the connection is closed automatically by Java.
* Interaction with time series data should be done via the TimeSeriesClient (not TimeSeriesRDBClient or TimeSeriesRDBClientWithReducedTables).
* The methods in the TimeSeriesClient are broadly separated into methods that receive a connection object and methods without the connection object. By using methods with the connection object, it is up to the developer to ensure that the connection is closed by using try-with-resources demonstrated.

Example using TimeSeriesRDBClient's built-in connection handling:
```java
TimeSeriesRDBClient<Instant> timeSeriesRdbClient = new TimeSeriesRDBClient<>(Instant.class);
timeSeriesRdbClient.setRdbUrl(url); timeSeriesRdbClient.setRdbUser(user); timeSeriesRdbClient.setRdbPassword(password);

RemoteStoreClient remoteStoreClient = new RemoteStoreClient(kgUrl, kgUrl);

TimeSeriesClient<Instant> timeSeriesClient = new TimeSeriesClient(remoteStoreClient, timeSeriesRdbClient);

timeSeriesClient.initTimeSeries(dataIRIs, dataClass, timeUnit);
timeSeriesClient.addTimeSeriesData(timeseries);
```
Note the setting of RDB details in the TimeSeriesRDBClient object, the same applies to TimeSeriesRDBClientWithReducedTables. Each method call will open and close a single connection, so this may not be suitable for applications that require lots of interaction with the database.

## TimeSeriesClientFactory
This class provides a method to generate a TimeSeriesClient object, `TimeSeriesClientFactory.getInstance(TripleStoreClientInterface storeClient, List<String> dataIriList)`, with the correct RDB client (either TimeSeriesRDBClient or TimeSeriesRDBClientWithReducedTables), schema, RDB URL and time class.

The provided IRIs need to be initialise with time series beforehand and the store client should point towards the SPARQL endpoint containing the time series instances.

Example:
```java
TimeSeriesClient<?> timeSeriesClient = TimeSeriesClientFactory.getInstance(remoteStoreClient, dataIriList);
```

## Time series instantiation
The namespaces used in this document:  
(`ts` denotes the time series ontology and `kb` refers to the namespace to which the time series shall be added)
```
ts  : https://www.theworldavatar.com/kg/ontotimeseries/
rdf : http://www.w3.org/1999/02/22-rdf-syntax-ns#
xsd : http://www.w3.org/2001/XMLSchema#
kb  : https://www.theworldavatar.com/kg/ontotimeseries/
time: http://www.w3.org/2006/time#
```

### Instantiation in KG ###
Upon instantiation of a time series for any `<entity>` in the KG, the following triples will be created depending on the subclass of time series:

<b>1. Instantaneous Time Series</b>
```
<entity>  ts:hasTimeSeries  kb:InstantaneousTimeSeries_UUID
kb:InstantaneousTimeSeries_UUID  rdf:type  ts:InstantaneousTimeSeries;
kb:InstantaneousTimeSeries_UUID  ts:hasRDB  "Postgres URL"
kb:InstantaneousTimeSeries_UUID  ts:hasTimeUnit  "timeUnit"
kb:InstantaneousTimeSeries_UUID  ts:hasSchema  "public"
kb:InstantaneousTimeSeries_UUID  ts:hasTimeClass  "java.time.Instant"
kb:InstantaneousTimeSeries_UUID  ts:hasRDBClientClass "uk.ac.cam.cares.jps.base.timeseries.___"
```

<b>2. StepwiseCumulative Time Series</b>
```
<entity>  ts:hasTimeSeries  kb:StepwiseCumulativeTimeSeries_UUID
kb:StepwiseCumulativeTimeSeries_UUID  rdf:type  ts:StepwiseCumulativeTimeSeries
kb:StepwiseCumulativeTimeSeries_UUID  ts:hasRDB  "Postgres URL"
kb:StepwiseCumulativeTimeSeries_UUID  ts:hasTimeUnit  "timeUnit"
kb:StepwiseCumulativeTimeSeries_UUID  ts:hasSchema  "public"
kb:StepwiseCumulativeTimeSeries_UUID  ts:hasTimeClass  "java.time.Instant"
kb:StepwiseCumulativeTimeSeries_UUID  ts:hasRDBClientClass "uk.ac.cam.cares.jps.base.timeseries.___"
```

<b>3. CumulativeTotal Time Series</b>
```
<entity>  ts:hasTimeSeries  kb:CumulativeTotalTimeSeries_UUID
kb:CumulativeTotalTimeSeries_UUID  rdf:type  ts:CumulativeTotalTimeSeries
kb:CumulativeTotalTimeSeries_UUID  ts:hasRDB  "Postgres URL"
kb:CumulativeTotalTimeSeries_UUID  ts:hasTimeUnit  "timeUnit"
kb:CumulativeTotalTimeSeries_UUID  ts:hasSchema  "public"
kb:CumulativeTotalTimeSeries_UUID  ts:hasTimeClass  "java.time.Instant"
kb:CumulativeTotalTimeSeries_UUID  ts:hasRDBClientClass "uk.ac.cam.cares.jps.base.timeseries.___"
```

<b>4. Average Time Series</b>
```
<entity>  ts:hasTimeSeries  kb:AverageTimeSeries_UUID
kb:AverageTimeSeries_UUID  rdf:type  ts:AverageTimeSeries
kb:AverageTimeSeries_UUID  ts:hasRDB  <Postgres URL>
kb:AverageTimeSeries_UUID  ts:hasTimeUnit  <timeUnit>
kb:AverageTimeSeries_UUID  ts:hasAveragingPeriod kb:AveragingPeriod_UUID
kb:AveragingPeriod_UUID    rdf:type  time:Duration
kb:AveragingPeriod_UUID    time:unitType <time:temporalUnit>
kb:AveragingPeriod_UUID    time:numericDuration <value>^^xsd:decimal
kb:AveragingPeriod_UUID    ts:hasSchema  "public"
kb:AveragingPeriod_UUID    ts:hasTimeClass  "java.time.Instant"
kb:AveragingPeriod_UUID    ts:hasRDBClientClass  "uk.ac.cam.cares.jps.base.timeseries.___"
```

<b>In case time series subclass is not specified, the following triples will be created:</b>
```
<entity>  ts:hasTimeSeries  kb:TimeSeries_UUID
kb:TimeSeries_UUID  rdf:type  ts:TimeSeries
kb:TimeSeries_UUID  ts:hasRDB  <Postgres URL>
kb:TimeSeries_UUID  ts:hasTimeUnit  <timeUnit>
kb:TimeSeries_UUID  ts:hasSchema  "public"
kb:TimeSeries_UUID  ts:hasTimeClass  "java.time.Instant"
kb:TimeSeries_UUID  ts:hasRDBClientClass  "uk.ac.cam.cares.jps.base.timeseries.___
```

The created `UUID` denotes a UUID version 4. The data properties `Postgres URL` and `timeUnit` are Literals describing the link to the RDB and the time format in which the data is stored, respectively.

The RDF type for each time series can be specified using the `Type` enum located in `TimeSeriesClient`. The enum can assume one of the following values:
<br />
`Type.GENERAL` corresponds to `ts:TimeSeries`<br />`Type.INSTANTANEOUS` corresponds to `ts:InstantaneousTimeSeries`<br /> 
`Type.STEPWISECUMULATIVE` corresponds to `ts:StepwiseCumulativeTimeSeries`<br /> `Type.AVERAGE` corresponds to `ts:AverageTimeSeries`<br />
`Type.CUMULATIVETOTAL` corresponds to `ts:CumulativeTotalTimeSeries`

The averaging period for average time series is linked to the `unit type` and the `numericDuration`.

The `unitType` describes the temporal unit of the averaging period and can assume one of the following values: `ChronoUnit.SECONDS`, `ChronoUnit.MINUTES`, `ChronoUnit.HOURS`,
`ChronoUnit.DAYS`, `ChronoUnit.WEEKS`, `ChronoUnit.MONTHS`, `ChronoUnit.YEARS`, which correspond to `unitSecond`, `unitMinute`, `unitHour`, 
`unitDay`, `unitWeek`, `unitMonth`, `unitYear` respectively. 

 The `numericDuration` describes the numerical value of the averaging period and must be a positive decimal. It is specified using Duration.ofDays(*number of Days*)
where *number of Days* will be converted to the `numericDuration` according to the specified `unitType`. 

Use the following conversions when creating the Duration object:
`1 month = 33 days` `1 year = 366 days`.

### Instantiation in RDB using TimeSeriesRDBClient ###
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

### Instantiation in RDB using TimeSeriesRDBClientWithReducedTables ###
Instead of creating a table for each time series instance, this class will use one table per time class and reuse existing columns when a new time series instance is added. The lookup table is similar:
| data_iri | time_series_iri | table_name | column_name |
| :---: | :---: | :---: | :---: |
| measure 1 | time series 1 | table 1 | column 1 |
| measure 2 | time series 1 | table 1 | column 2 |
| measure 3 | time series 2 | table 1 | column 1 |
| measure 4 | time series 2 | table 1 | column 2 |
| measure 5 | time series 2 | table 1 | column 3 |

The table storing the time series data contains an extra time series IRI column to filter the time series:
| time | column 1 | column 2 | column 3 | time series |
| :--: | :--: | :--: | :--: | :--: |
| t1 | ... | ... | null | time series 1 |
| t2 | ... | ... | null | time series 1 |
| t1 | ... | ... | ... | time series 2 |
| t2 | ... | ... | ... | time series 2 |

Due to the way different time series share the same table, there may be a lot of table entries being null if the time series have different data types or number of data.

## Examples on how to use the TimeSeriesClient ##
- **Integration tests**:
Detailed integration tests for the `TimeSeriesClient` as well as the (underlying) `TimeSeriesRDBClient` and `TimeSeriesSparql` are provided in the respective [test repository]. Please note that all integration tests use the Testcontainers Java library and, hence, require Docker to be installed. Furthermore, access to the `docker.cmclinnovations.com registry` is required from the machine the test is run on to pull docker images.  
You can request login details by emailing `support<at>cmclinnovations.com` with the subject 'Docker registry access'
   
- **Agent examples**:
   Several agents provide working examples of how to use the `TimeSeriesClient`, e.g.  
   * [AQMeshInputAgent] queries and stores time series data from AQ Mesh sensor in Singapore (Java)
   * [FloodAgent] queries water level data from the Environment Agency, stores it in the KG, and retrieves it for visualisation (Java)
   * [TimeSeriesExample] provides a minimum working example on how to instantiate time series data which is attached to some geospatial reference, stores it in the KG, and retrieves it for visualisation (Python, access of JPS_BASE_LIB via py4jps)
   * [GasGridAgent] queries instantaneous gas flow data from the National Grid, stores it in the KG, and retrieves it for visualisation (Python, access of JPS_BASE_LIB via py4jps)

[//]: # (These are reference links used in the body)

   [test repository]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/test/java/uk/ac/cam/cares/jps/base/timeseries>
   [AQMeshInputAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AQMeshInputAgent>
   [FloodAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FloodAgent>
   [GasGridAgent]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/1161-dev-gas-grid-input-agent/Agents/GasGridAgent/>
   [TimeSeriesExample]: <https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/TimeSeriesExample>
