# Time Series Example

This example provides a minimum working example on how to instantiate time series data which is attached to some geospatial reference within the KG using the [TimeSeriesClient]. Furthermore, it shows how to query and visualise this data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] code base (mainly) written in Java, the [py4jps] Python wrapper is used.

## Preparation
### 1. Create a Python virtual environment and install required packages (in Windows):

1) Open `cmd` terminal and navigate into project repository

2) Create virtual environment `name` using venv (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variable):
```
python -m venv <name>
```

3) Activate virtual environemnt by running:
```
<name>\Scripts\activte
```

4) Install requirements listed in `requirements.txt`:
```
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

### 2. Setup local Blazegraph instance and PostgreSQL database 

Start up Blazegraph and create a new namespace. Populate the `sparql.query.endpoint` and `sparql.update.endpoint` fields in the [requirements file] with the respectively SPARQL endpoints to this namespace.

Start pgAdmin and create a new PostgreSQL database (e.g. `<ts_example>`). Update the fields `db.url`, `db.user`, and `db.password` according to your local PostgreSQL settings. The default URL (with default port) for the example database name would be `jdbc:postgresql:ts_example`. 

## Usage

The example uses the following namespaces:
```
# ABoxes
ex   : http://www.theworldavatar.com/kb/ts_example/
tsa  : http://www.theworldavatar.com/kb/ontotimeseries/
# Tboxes (ontologies)
rdf  : http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs : http://www.w3.org/2000/01/rdf-schema#
ts   : https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#
xsd  : http://www.w3.org/2001/XMLSchema#
geo  : http://www.bigdata.com/rdf/geospatial/literals/v1#
```

### 1. Instantiate Geospatial Time Series Data

Assumptions:
- Elevation (Z coordinate) neglected
- Coordinates in EPSG:4326 to use Blazegraph's built-in geospatial search functionality

Upon instantiation, the following triples get created for each `dataIRI`:
```
ex:consumerIRI rdf:type ex:Consumer ;
               rdfs:label <description> ; 
               ex:consumes ex:dataIRI ; 
               ex:hasLocation <lat#lon> .
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


### 3. Visualise Geospatial Time Series Data using the DTVF

Digital Twin Visualisation Framework ([DTVF])





[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[py4jps]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_wrapper
[requirements file]: resources/ts_example.properties