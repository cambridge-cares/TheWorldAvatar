# Time Series Example

This example provides a minimum working example on how to instantiate time series data which is attached to some geospatial reference within the KG using the [TimeSeriesClient]. Furthermore, it shows how to query and visualise this data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

This example assumes that both Blazegraph and PostgreSQL are available locally. Furthermore, for PostgreSQL it is assumed that default connection settings are used, i.e. _localhost_ as `host` and _5432_ as `port`.

## Preparation
### 1. Create a Python virtual environment and install required packages (in Windows):

1) Open `cmd` terminal and navigate into project's root repository (referred to as `<root>` in the following)

2) Create virtual environment `<venv_name>` using venv (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variables):
```
python -m venv <venv_name>
```

3) Activate virtual environment by running:
```
<venv_name>\Scripts\activate
```

4) Install requirements listed in `requirements.txt`:
```
python -m pip install --upgrade pip  
python -m pip install -r requirements.txt
```

### 2. Setup local Blazegraph instance and PostgreSQL database 

Start up Blazegraph and ensure that the local instance is available (i.e. via Blazegraph workbench in the Browser). Populate the `sparql.query.endpoint` and `sparql.update.endpoint` fields in the [properties file] with the wanted SPARQL endpoints. Please note that the specified namespace will be created automatically by the `instantiate_data.py` script. In other words, if the specified SPARQL endpoint is `http://localhost:9999/blazegraph/namespace/<ts_example>/sparql`, the program will automatically create the namespace `<ts_example>` with geospatial capability enabled before uploading any triples. In case the respective namespace already exists, a message is printed out and the namespace should be deleted manually (if required).

Start postgreSQL and ensure that the local server is available (i.e. via pgAdmin ). Update the fields `db.url`, `db.user`, and `db.password` in the [properties file] according to your local PostgreSQL settings. The default URL (with default `host` (i.e. _localhost_) and `port` (i.e. _5432_)) looks like `jdbc:postgresql:<database_name>`. Please note that the specified database `<database_name>` will be created automatically by the `instantiate_data.py` script. In case the respective database already exists, a message is printed out and the database should be deleted manually (if required).

**Please note**: It is recommended to populate/adjust all fields in the [properties file] (**except** for the `output.directory`) before executing any of the scripts below.

## Usage

The example uses the following namespaces:
```
# ABoxes
ex     : http://www.theworldavatar.com/kb/ts_example/
tsa    : http://www.theworldavatar.com/kb/ontotimeseries/
# Tboxes (ontologies)
rdf    : http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs   : http://www.w3.org/2000/01/rdf-schema#
ts     : https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#
xsd    : http://www.w3.org/2001/XMLSchema#
geolit : http://www.bigdata.com/rdf/geospatial/literals/v1#
geo    : http://www.bigdata.com/rdf/geospatial#
```

### 1. Instantiate Geospatial Time Series Data

All coordinates in the sample date are in EPSG:4326 to use Blazegraph's built-in geospatial search functionality.

To instantiate the sample data, please run (from within the `<root>` directory and with **activated** virtual environment `<venv_name>`)
```
python instantiate_data.py
```
Upon instantiation, the following triples get created for each `dataIRI`:
```
ex:consumerIRI rdf:type ex:Consumer ;
               rdfs:label <description> ; 
               ex:consumes ex:dataIRI ; 
               ex:hasLocation <lat#lon> ;
               ex:hasGeometry <lat#lon#elevation coordinate list> .
ex:dataIRI rdf:type ex:Consumption ;
           rdfs:label <description> ;
           ex:unit <unit> .
           ts:hasTimeSeries  tsa:tsIRI .
tsa:tsIRI rdf:type ts:TimeSeries ;  
          ts:hasRDB  <Postgres URL> ;
          ts:hasTimeUnit  <timeUnit> .
```
A `consumerIRI` describes any entity which consumes a particular utility (e.g. Pembroke College). A `dataIRI` refers to such a utility consumption, which can be described with a time series (e.g. electricity consumption). And the `tsIRI` denotes the actual time series data, which reflects the behaviour of a `dataIRI` (i.e. holds the actual timestamps and values).

### 2. Query Geospatial Time Series Data

A valid Mapbox API key (which can be obtained for free by signing up) is required for later data visualisation and must be provided in the [properties file] **before** querying the data.

To query the triple store and write the retrieved data to respective files for later visualisation, please run (from within the `<root>` directory and with **activated** virtual environment `<venv_name>`)
```
python query_data.py
```

Within the main function of this module, one can specify, whether to query for all consumers or just consumers within a certain radius from Cambridge's city center (around line 250).

### 3. Visualise Geospatial Time Series Data using the DTVF

The previously queried data will be visualised using the Digital Twin Visualisation Framework (DTVF). A brief introduction on how to use this framework is provided below; however, for more detailed instructions, please read the [DTVF] TWA wiki page.

#### 3.1. Building the Image

Before building the image, please start up Docker Desktop (Previous users reported potential issues in case Docker Desktop is not run as Administrator). 

The `<root>\dtvf_visualisation\docker` folder contains the required files to build a Docker Image for the example visualisation. The `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The example visualisation within the Docker image will be based on the current content of the `<root>\dtvf_visualisation\queried_data` repository at the point of building the image. This is also the repository to which all queried data files are written automatically  if the `output.directory` field in the [properties file] is not changed.
* A connection to the internet is required to contact remote resources and use the mapping libraries.
* At the time of writing, the JS and CSS files comprising the framework are stored within the  visualisation folder (in the `js` and `css` directories). In the future, these files will be separated from the data, hosted remotely (perhaps on the kg.cmclinnovations.com site), then imported as remote resources in any visualisation.

#### 3.2. Docker Commands
Once the requirements have been addressed, the Image can be built using the following commands (to be run in CMD from within the `<root>\dtvf_visualisation` repository and with **deactivated** virtual environment `<venv_name>`):

+ To build the Image:
  + `docker-compose -f ./docker/docker-compose.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`

#### 3.3. Afterwards the visualisation can be viewed via Docker Desktop 
In Docker Desktop:
1. Select "Container/Apps" in left panel
2. Select container "dtvf_example_visualisation" in docker stack "docker"
3. Select `Open in Browser` to open visualisation in Browser (in case the visualisation does not automatically show up, please refresh the page. If nothing is shown on any web browser, you can open the visualisation at [http://localhost:65080/])



[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[py4jps]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_wrapper
[properties file]: resources/ts_example.properties
[http://localhost:65080/]: http://localhost:65080/
