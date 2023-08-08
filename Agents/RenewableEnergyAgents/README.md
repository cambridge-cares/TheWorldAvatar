# RenewableEnergyAgents Project

This project instantiates wind and solar sensor generated time series data which is attached to geospatial reference within the World Avatar Knowledge Graph using the [TimeSeriesClient]. It queries the Knowledge Graph and visualises this data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

This project assumes that both Blazegraph and PostgreSQL are available locally. Furthermore, for PostgreSQL it is assumed that default connection settings are used, i.e. _localhost_ as `host` and _5432_ as `port`.

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

Start up Blazegraph and ensure that the local instance is available (i.e. via Blazegraph workbench in the Browser). Populate the `sparql.query.endpoint` and `sparql.update.endpoint` fields in the [properties file] with the wanted SPARQL endpoints. Please note that the specified namespace will be created automatically by the `instantiate_data.py` script. In other words, if the specified SPARQL endpoint is `http://localhost:9999/blazegraph/namespace/renewable_energy/sparql`, the program will automatically create the namespace `renewable_energy` with geospatial capability enabled before uploading any triples. In case the respective namespace already exists, a message is printed out and the namespace.

Start postgreSQL and ensure that the local server is available (i.e. via pgAdmin ). Update the fields `db.url`, `db.user`, and `db.password` in the [properties file] according to your local PostgreSQL settings. The default URL (with default `host` (i.e. _localhost_) and `port` (i.e. _5432_)) is `jdbc:postgresql:renewable_energy`. Note that `renewable_energy` is the name of the database that will be created automatically by the `Utils.utils.py` script. In case the respective database already exists, a message is printed out and the database should be deleted manually (if required).

## Usage

The project uses the following namespaces for the knowledge graph consisting of both TBoxes and ABoxes:

```
# KG
rdf    : http://www.w3.org/1999/02/22-rdf-syntax-ns#
rdfs   : http://www.w3.org/2000/01/rdf-schema#
ts     : https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#
xsd    : http://www.w3.org/2001/XMLSchema#
geolit : http://www.bigdata.com/rdf/geospatial/literals/v1#
geo    : http://www.bigdata.com/rdf/geospatial#
ssn    : https://www.w3.org/TR/vocab-ssn/,
ontoenergysystem : https://theworldavatar.com/kg/ontoenergysystem/,
ontoweather: https://www.theworldavatar.com/kg/ontoweather/,
ontoturbine: https://www.theworldavatar.com/kg/ontoturbine/,
ontosolarpanel: https://www.theworldavatar.com/kg/ontosolarpanel/,
ontoenergy: https://www.theworldavatar.com/kg/ontoenergy/,
om: http://www.ontology-of-units-of-measure.org/resource/om-2/,
sosa: http://www.w3.org/ns/sosa

```

### 1. Instantiate and Query Geospatial Time Series Data

Download the [data folder] and put it at the same level of Agents\RenewableEnergyAgents. Follow the instructions provided in the README.md file of each agent listed below to instantiate and query related geospatial time-series data.

* MetOfficeSolarSensorAgent
* MetOfficeWindSensorAgent
* UrbanObservatorySolarSensorAgent
* UrbanObservatoryWindSensorAgent
* PostcodeSolarEnergyAgent
* PostcodeWindEnergyAgent

### 2. Visualise Geospatial Time Series Data using the DTVF

The previously queried data by an agent will be visualised using the Digital Twin Visualisation Framework (DTVF). A brief introduction on how to use this framework is provided below; however, for more detailed instructions, please read the [DTVF] TWA wiki page.

#### 2.1. Building the Image

Before building the image, please start up Docker Desktop (Previous users reported potential issues in case Docker Desktop is not run as Administrator). 

The `<root>\dtvf_visualisation\docker` folder contains the required files to build a Docker Image for the example visualisation. The `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The visualisation within the Docker image will be based on the current content of the `<root>\dtvf_visualisation\queried_data` repository at the point of building the image. This is also the repository to which all queried data files are written automatically  if the `output.directory` field in the [properties file] is not changed.
* A connection to the internet is required to contact remote resources and use the mapping libraries. [Create a Mapbox API access token] if not done yet and replace <PUT_YOUR_KEY_HERE> with the token in the files Agents/RenewableEnergyAgents/dtvf_visualisation/index.html and Agents/RenewableEnergyAgents/resources/renewable_energy_agents.properties in order for the visualisation to work.
* At the time of writing, the JS and CSS files comprising the framework are stored within the  visualisation folder (in the `js` and `css` directories). In the future, these files will be separated from the data, hosted remotely (perhaps on the kg.cmclinnovations.com site), then imported as remote resources in any visualisation.

#### 2.2. Docker Commands
Once the requirements have been addressed, the Image can be built using the following commands (to be run in CMD from within the `<root>\dtvf_visualisation` repository and with **deactivated** virtual environment `<venv_name>`):

+ To build the Image:
  + `docker-compose -f ./docker/docker-compose.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`
cd chemeng2\researchproject\researchprojectcode\renewableenergy\dtvf_visualisation

#### 2.3. Afterwards the visualisation can be viewed via Docker Desktop 
In Docker Desktop:
1. Select "Container/Apps" in left panel
2. Select container "dtvf_visualisation" in docker stack "docker"
3. Select `Open in Browser` to open visualisation in Browser (in case the visualisation does not automatically show up, please refresh the page)



[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[TimeSeriesClient]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries
[py4jps]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper
[properties file]: resources/renewable_energy_agents.properties
[Create a Mapbox API access token]: https://account.mapbox.com/access-tokens/
[data folder]: https://www.dropbox.com/sh/2dgpwmedboumkkt/AAAPUxMSa5BTw10iPVkReBGaa/Codes/Research%20project%20code?dl=0&subfolder_nav_tracking=1