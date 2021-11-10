# Time Series Example

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

### 1. Instantiate Geospatial Time Series Data

Assumptions:
- Elevation (Z coordinate) neglected
- Coordinates in EPSG:4326 to use Blazegraph's built-in geospatial search functionality
- 


### 2. Query Geospatial Time Series Data


### 3. Visualise Geospatial Time Series Data using the DTVF

Digital Twin Visualisation Framework ([DTVF])






[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[requirements file]: resources/ts_example.properties