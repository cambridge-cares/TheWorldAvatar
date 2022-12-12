# Historical Pump Data Instantiation Agent

Using the JPS-base-lib's time-series client, this agent extracts time series data from an Excel workbook, and stores them 
and their corresponding instances into the knowledge graph (KG) and a relational database (RDB).

When called for the first time, it will initialize the KG and RDB. Subsequently, only new data will be updated or inserted.
More details on the classes, public methods, and documentation is available in the source code.

At present, this agent can only retrieve annual time series from the Excel, and convert them to Instant.

## Instructions
### 1. Excel preprocessing
This agent is designed to work with any Excel version in .xls or .xlsx format. An Excel workbook must be placed in the
`data/` directory. Only one workbook will be processed at a time. Please do not put more than one workbook for processing.

A snippet of a sample Excel is as follows:

```
| Group     | Year | Property  | Property 2 |
--------------------------------------------
| Pump 1    | 2000 | 10515     | 10256.10   |
| Pump 1    | 2001 | 10425     | 82275.15   |
| Pump 2    | 2000 | 7261      | 23456.70   |
| Pump 2    | 2001 | 20425     | 52472.45   |
| Pump 3    | 2000 | 10514     | 46258.10   |
| Pump 3    | 2001 | 35192     | 33302.05   |
```

Some pre-processing might be required in order to ensure that the Excel content are compatible with the agent. 

First, change the time header to Year. At the moment, only `Year` or `TimeStamp` are accepted as an input. 
Time stamps should be in `YYYY-MM-DD HH:MM:SS` format.

Second, if there are any groups of time series, sort the column containing multiple time series groups ie `Group`.

Notes:
- .csv files are incompatible at this moment. An alternate solution is to copy and paste the .csv content into an Excel sheet.
- All numeric data are return as `Double.class` or `Date.class` in Java at the moment.
- Contents do not need to start at the first sheet or second row. This can be set in the `HistoricalPumpDataInstantiationAgent` class/ POST request.

### 2. Building the Agent
This agent is designed to be an executable war and deployed as a web servlet on Tomcat. Then, a POST request would
need to be sent to the specific URL to initiate the agent. This is achieved using the Docker platform. Alternate
build processes are not within this document's scope.  

#### 2.1 Preparation
##### 2.1.1 Credentials
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide  your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

#### 2.2 Docker Deployment
Deploy the agent and its dependencies by running the following code in the command prompt at the `<root>` directory:
```
docker-compose up -d
```

#### 2.3 Stack Deployment

If you want to spin up this agent as part of a stack, do the following:
- Copy the contents of `config/client.properties_stack` into `config/client.properties`, inserting the name of your stack.
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into the `inputs/config` folder of the stack manager, adjusting the absolute path of the bind mounts as required.
- Start the stack manager as usual. This should start the container.

### 3. Running the Agent
#### 3.1 Precursor
In the `config` directory, please place the `client.properties` file containing the access details for the KG and RDB.

In the `root/data` directory, please place the Excel workbook. If there is a preceding mapping file,
it must be named as `excel.properties`. Note that the mapping file will be automatically generated otherwise.

##### 3.1.1 KG and RDB Endpoint access file
Access to a KG SPARQL database and RDB (PostgreSQL) endpoint from the host machine is required.
The access port is either local or external, and must be running on a server or from a Docker container.
It is not within this document's scope to explain the set-up of a KG or RDB database.

These access ports are defined in a `.properties` file for the time-series client to function. This should be placed in
the `<root>/config` directory, preferably named as `client.properties`. A sample file have been provided for illustration.
It should contain the following credentials and endpoints:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database ^
    - Please specify the IP address to grant easy access to the time series for production. Do not use `localhost` or `host.docker.internal` for production
- `db.user` the username to access the PostgreSQL database
- `db.password` the password to access the PostgreSQL database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph ^
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph ^

^ Please ensure that the KG namespace and RDB database name has been created and specified accordingly.

##### 3.1.2 Mapping File
The mapping file define how data retrieved from Excel is related to the KG and RDB. Specifically, each column represents
a specific measure, that needs to be represented as a class in the KG through a data IRI. This is achieved through a
properties file, containing one line per column and linked to an IRI. **The mapping must be 1-1** ie no dataIRI should
be linked to multiple columns or vice versa.

The agent will automatically generate this file from the Excel if it is missing on the first build process. If there is
a preceding  properties file with established dataIRI, please place the `excel.properties` file in the `data`
directory to retain the relations. When the IRI is left empty, the agent will generate one and save the changes to file.

```
// dataIRI format 
[prefix]/[columnName]_[UUID]

// Sample line 
oilconsumption=http://example/oilconsumption_150252

// Sample empty IRI
oilconsumption=

// For group timeseries
pump1_oilconsumption = http://example/pump1_oilconsumption_150252
```
*Note that the `[prefix]` is modifiable by the `iriPrefix` key in the POST Request.

*If any mappings are modified from preceding builds, any data inputs are perceived as a *"new"* time series. This can result
in inconsistencies in both KG and RDB. In such circumstances, please reset the namespaces and database to  a clean slate.

*The mapping file can be retrieved from the `data` directory.

#### 3.2 POST Request Parameters
The agent currently accepts two parameters.

1. Time Header - Mandatory

This is the header name for the Time column for the Time Series. It can be invoked with the `timeHeader` key. 
At the moment, only `Year` and `TimeStamp` is accepted. 

2. IRI Prefix - Mandatory

By default, the base IRI prefix declared for all time series is `https://www.theworldavatar.com/kg/ontotimeseries/`.
If you wish to append a namespace, pass a String of `namespace/` or `namespace/part2/` to the `iriPrefix` key. 
Note that it must start with no `/` but end with a `/`.

To change the base IRI for time series, pass in a String in `http://www.example.com/namespace/` format to the `iriPrefix` key.
Both `http` and `https` is acceptable, but it must end with `/`.

3. Supplementary Triples - Mandatory

This is a `boolean` parameter that determines if supplementary triples should be added. Please invoke this with
the `addTriple` and a `False` value for most cases. This is only intended to supplement one specific Excel file,
and in that case, parameter should be `True`.

4. Starting Value Row - Optional

This is the starting row for values, and should be an integer in String format. By default, it is set to 1. 
If you wish to start at other values larger than 1, invoke the parameter with the `startingRow` key.

5. Column Index for Multiple Time Series - Optional

This is the column containing the group for multiple time series. For eg, if an Excel contains data for "Pump 1" and "Pump 2",
each pump should have their own distinct time series. Pump 1 and 2 will be indicated in this column. See [sample Excel](#1-excel-preprocessing) 
for more details. 

For data that does not contain multiple time series, do not invoke this parameter. 
If required, pass an Integer (more than 0) in string format with the `multiTSColIndex` key. Note that the index starts from 0 for first column.

#### 3.3 POST Request
Run the agent by sending a POST request with the required JSON Object to ` http://localhost:4050/historical-pump-data-instantiation-agent/run`.
At least one parameter is required. A sample request is as follows:
```
POST http://localhost:4050/historical-pump-data-instantiation-agent/run
Content-Type: application/json
{"timeHeader":"Year", "iriPrefix":"pump/", "addTriple":"False", startingRow": "1", "multiTSColIndex": "0"}

// Written in curl syntax (as one line)
curl -X POST --header "Content-Type: application/json" -d "{'timeHeader':'Year', 'iriPrefix':'pump/', 'addTriple':'False', 'startingRow': '1', 'multiTSColIndex': '0'}" http://localhost:4050/historical-pump-data-instantiation-agent/run
```
If the agent ran successfully, a JSON Object would be returned as follows:
```
{"Result":["Data updated with new readings from Excel Workbook.","Timeseries Data has been updated."]}
```
### 4 Post-Build
The instantiated data will be available in the KG and RDB, and requires their own queries to access. The mapping files can be retrieved from the `data` directory.