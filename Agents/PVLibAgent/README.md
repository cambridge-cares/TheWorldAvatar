# PVLib Agent

This agent is designed to calculate estimated AC and DC Power based on values provided in the properties files and values
queried from the knowledge graph. It will then initialise the AC and DC Power as timeseries in the knowledge graph. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS_BASE_LIB to interact with both the knowledge graph and database and uses [PvLib](https://pvlib-python.readthedocs.io/en/stable/)
for it's AC and DC Power calculations.

## Usage 
This part of the README describes the usage of the agent.   

### Property files
For running the agent, three property files are required:
- One [property file for DC and AC Power instantiation](#dataIRIs-properties) defining the IRIs for each of the keys.
- One [property file for the time-series client](#time-series-client-properties) defining timeseries client related parameters.
- One [property file for the Solar Model](#model-parameters-properties) defining the parameters of the Solar PV Model.

#### dataIRIs properties
This property file is used to determine whether there are IRIs already created for the instantiation of DC and AC Power
in the knowledge graph. This property file can be left unchanged where the agent will then create IRIs for the keys indicated
in the file. More information can be found in the property file `dataIRIs.properties` in the `resources` folder.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.query.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database to query from
- `db.query.user` the username to access the Postgres database to query from
- `db.query.password` the password to access the Postgres database to query from
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `db.update.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database to update to
- `db.update.user` the username to access the Postgres database to update to
- `db.update.password` the password to access the Postgres database to update to
- `air.temperature.iri` the timeseries data IRI for air temperature, see [Prerequisites](#prerequisites) for more details
- `wind.speed.iri` the timeseries data IRI for wind speed, see [Prerequisites](#prerequisites) for more details
- `irradiance.iri` the timeseries data IRI for irradiance, see [Prerequisites](#prerequisites) for more details
- `iri` the IRI of the sensor or weather station entity that has rdf:type s3n:SmartSensor or rdf:type ontoems:ReportingStation, see [Prerequisites](#prerequisites) for more details

More information can be found in the property file `ts_client.properties` in the `resources` folder.

#### Model parameters properties
The model_parameters properties contains the parameters needed to create a solar PV Model for calculations. It should contain the following keys:
- `temp_model` the temperature model (sapm or pvsyst)
- `temp_model_config` the temperature model configuration (for sapm: open_rack_glass_glass or close_mount_glass_glass or open_rack_glass_polymer or insulated_back_glass_polymer, for pvsyst: freestanding or insulated)
- `latitude` the latitude of the Solar PV Model location, can be left empty in which it will be queried from the knowledge graph
- `longitude` the longitude of the Solar PV Model location, can be left empty in which it will be queried from the knowledge graph
- `altitude` the altitude of the Solar PV Model location
- `surface_tilt` the surface tilt angle of the Solar PV Model
- `surface_azimuth` the surface azimuth of the Solar PV Model
- `module_rated_dc_power` the rated DC power of the PV module
- `module_temperature_coefficient` the temperature coefficient of the PV module
- `inverter_rated_dc_power` the rated DC power of the inverter
- `modules_per_string` the number of modules per string
- `strings_per_inverter` the number of strings per inverter

More information can be found in the property file `model_parameters.properties` in the `resources` folder.

The [next section](#prerequisites) will explain the requirements to run the agent.

### Prerequisites
1. It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL.
This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

2. The agent is designed to work with weather stations and irradiance sensors. It is necessary to either have a weather station that is measuring
air temperature, wind speed and global horizontal irradiance and be able to retrieve the measurements from the weather station or have an irradiance
sensor that is measuring irradiance and be able to retrieve the measurements from the sensor.

For weather stations:
It will query for the latest air temperature, wind speed and global horizontal irradiance values from the knowledge graph.
Thus, it is necessary to have the air temperature, wind speed and global horizontal irradiance initialised as timeseries via the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
beforehand. In addition, the weather station and its related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_weatherStation> rdf:type ontoems:ReportingStation .
<http://test_weatherStation> ontoems:reports <http://test_parameter> .
<http://test_parameter> rdf:type ontoems:AirTemperature .
<http://test_parameter> om:hasValue <air temperature data IRI> .
```
see [OntoEMS ontology](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoems) for more information.

In the events that the instantiation of the weather station and its related timeseries data does not follow the structure above,
the timeseries data IRIs for all three parameters have to be indicated in the `ts_client.properties` file. See [property file for the time-series client](#time-series-client-properties).

For Sensor:
It will query for the latest irradiance value from the knowledge graph. Thus, it is necessary to have the irradiance initialised as timeseries via the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
beforehand. In addition, the sensor and its related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_device> rdf:type saref:Device .
<http://test_device> rdf:type s3n:SmartSensor .
<http://test_device> saref:measuresProperty <http://test_property> .
<http://test_property> rdf:type saref:Property .
<http://test_property> rdf:type om:Irradiance .
<http://test_property> om:hasValue <irradiance data IRI> .
```
See [Saref ontology](https://saref.etsi.org/), [s3n ontology](https://recherche.imt-atlantique.fr/info/ontologies/sms/s3n/)
and [om ontology](http://www.ontology-of-units-of-measure.org/page/om-2) for more information.

In the events that the instantiation of the sensor and its related timeseries data does not follow the structure above,
the timeseries data IRI for irradiance have to be indicated in the `ts_client.properties` file. See [property file for the time-series client](#time-series-client-properties).

3. For latitude and longitude, these values can be queried from the knowledge graph if they have been instantiated with the following structure:
```
<http://device_entity> geo:location <http://test_location> .
<http://test_location> rdf:type	<https://www.w3.org/2003/01/geo/wgs84_pos#SpatialThing> .
<http://test_location> geo:lat <http://test_latValue> .
<http://test_location> geo:long <http://test_longValue> .
<http://test_latValue> rdf:type	<http://www.ontology-of-units-of-measure.org/resource/om-2/EclipticLatitude> .
<http://test_latValue> om:hasValue <http://test_Measure1> .
<http://test_Measure1> om:hasNumericalValue '1.3' .
<http://test_longValue> rdf:type <http://www.ontology-of-units-of-measure.org/resource/om-2/EclipticLongitude> .
<http://test_longValue> om:hasValue <http://test_Measure2> .
<http://test_Measure2> om:hasNumericalValue '130' .
```
In the events that latitude and longitude are not instantiated in the knowledge graph with the structure above, they have to
be included in the `model_parameters.properties` file. See [property file for the Solar Model](#model-parameters-properties).

### Building the Agent

Modify `dataIRIs.properties`, `model_parameters.properties` and `ts_client.properties` in the `resources` folder accordingly. 
Refer to [dataIRIs.properties description](#dataIRIs-properties), [model_parameters.properties description](#model-parameters-properties), [ts_client.properties description](#time-series-client-properties)
and [Prerequisites](#prerequisites) for more information.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

#### Run the agent
To run the agent, there are two options, the first is for weather stations:
```
GET http://localhost:1020/api/v1/evaluate?device=weatherStation
```
In curl syntax
```
curl -X GET http://localhost:1020/api/v1/evaluate?device=weatherStation
```
The second is for sensors:
```
GET http://localhost:1020/api/v1/evaluate?device=sensor
```
In curl syntax
```
curl -X GET http://localhost:1020/api/v1/evaluate?device=sensor
```
If the agent runs successfully, you should see a returned Object that is similar to the one shown below.
```
{"AC Power(W)":7.922936051747742,"DC Power(W)":9.510628810971978,"timestamp":"Thu, 24 Nov 2022 09:54:51 GMT"}
```

### Agent tests
Several unit tests are provided in the `tests` folder. Some tests are currently commented out as they required both 
Blazegraph and PostgreSQL to be running. To run those tests, the provided docker-compose.test.yml file can be used 
to spin up these services at the specified endpoints before uncommenting them. To run the tests, please follow the steps below:

1. Create a virtual environment with the following commands:
```
python -m venv pvlib_venv
pvlib_venv\Scripts\activate.bat
```
2. Install all required packages in virtual environment:
```
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```
3. Deploy Blazegraph and Postgresql as docker containers, uncomment and run the tests:
```
# Uncomment integration tests and start Docker services (if wanted)
docker compose -f "tests\docker-compose.test.yml" up -d --build 
# Run tests
pytest
```
