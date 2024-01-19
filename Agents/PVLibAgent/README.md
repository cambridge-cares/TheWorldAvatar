# PVLib Agent

This agent is designed to calculate AC and DC Power output from Photovaltaic Panels based on values provided in the properties files or values queried from the knowledge graph. It will then initialise the AC and DC Power as timeseries in the knowledge graph. The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) from the JPS_BASE_LIB to interact with both the knowledge graph and database and uses [PvLib](https://pvlib-python.readthedocs.io/en/stable/) for it's AC and DC Power calculations.

For the agent to read data, three property files are required:
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


## 1. PV Model Data Preparation
####  [Option 1] Read photovoltaic model specs from property files

##### Model  properties
The model_parameters properties contains the parameters required to create a solar PV Model for calculations. The file `model_parameters.properties` can be found in the `resources` folder. It should contain the following keys:
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

####  [Option 2] Read Photovoltaic Model Specs from Knowledge Graph
Alternatively, the parameters required to create a solar PV Model can be read from a knowledge graph. This requires an instantiation agent to create a Knowledge Graph filled with PV model parameter values. The  [HistoricalNTUEnergy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1496-dev-instantiate-historic-ntuenergyconsumptiondata-2/Agents/HistoricalNTUEnergyAgent) provides an example to instantiate a knowledge graph which includes PV model parameters.

## 2. Weather Data Preparation
The agent is designed to work with data from one of three sources: weather stations, irradiance sensors, and the OpenMeteo API. It is necessary to have one of the above data retrieved and instantiated on the knowledge graph before running the agent.

####  [Option 1] Read data from Weather Station
In the event that the weather data is retrieved from the weather station, the related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_weatherStation> rdf:type ontoems:ReportingStation .
<http://test_weatherStation> ontoems:reports <http://test_parameter> .
<http://test_parameter> rdf:type ontoems:AirTemperature .
<http://test_parameter> om:hasValue <air temperature data IRI> .
```
see [OntoEMS ontology](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoems) for more information. The [NUSDavisWeatherStation Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/NUSDavisWeatherStationAgent) provides an example of the instantiation.

The PVLib Agent will query for the latest air temperature, wind speed and global horizontal irradiance values from the knowledge graph.

In the events that the instantiation of the weather station and its related timeseries data does not follow the structure above, the timeseries data IRIs for all three parameters have to be indicated in the `ts_client.properties` file. See [property file for the time-series client](#time-series-client-properties).

Once It is instantiated, required to have access to a knowledge graph SPARQL endpoint.

The [next section](#prerequisites) will explain the requirements to run the agent.

####  [Option 2] Read data from Irradiance Sensor
In the event that the weather data is retrieved from the irradiance sensor, the related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_device> rdf:type saref:Device .
<http://test_device> rdf:type s3n:SmartSensor .
<http://test_device> saref:measuresProperty <http://test_property> .
<http://test_property> rdf:type saref:Property .
<http://test_property> rdf:type om:Irradiance .
<http://test_property> om:hasValue <irradiance data IRI> .
```
See [Saref ontology](https://saref.etsi.org/), [s3n ontology](https://recherche.imt-atlantique.fr/info/ontologies/sms/s3n/) and [om ontology](http://www.ontology-of-units-of-measure.org/page/om-2) for more information. The [Thingspeak Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ThingspeakAgent) provides an example of the instantiation.

In the events that the instantiation of the sensor and its related timeseries data does not follow the structure above, the timeseries data IRI for irradiance have to be indicated in the `ts_client.properties` file. See [property file for the time-series client](#time-series-client-properties).

####  [Option 3] Read data from OpenMeteo API
In the event that the weather data is retrieved from the irradiance sensor, the related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_openmeteo> rdf:type ontoems:ReportingStation .
<http://test_openmeteo> ontoems:reports <http://test_parameter> .
<http://test_parameter> rdf:type ontoems:AirTemperature .
<http://test_parameter> om:hasValue <air temperature data IRI> .
```
The [OpenMeteo Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OpenMeteoAgent) provides an example of the instantiation.

The PVLib Agent will query for the latest air temperature, wind speed and global horizontal irradiance values from the knowledge graph.

#### [Extra] Longitude and Magnitude data
For latitude and longitude, these values can be queried from the knowledge graph if they have been instantiated with the following structure:
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
In the events that latitude and longitude are not instantiated in the knowledge graph with the structure above, they have to be included in the `model_parameters.properties` file. See [property file for the Solar Model](#model-parameters-properties).

## 3. Build and Run
The agent is designed to run in two modes, either as a standalone docker container or work within a stack. The build and setup procedures are different for different running modes.

####  [Option 1] As a standalone docker container
Before running the agent in a standalone docker container, it is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL. However, it is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

Once a triple store and a Postgres database has been set up, modify the  `dataIRIs.properties`, `model_parameters.properties` and `ts_client.properties` in the resources folder accordingly. Refer to the property file descriptions for more information.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```
Select from one of the following to read weather data:
-  Read data from Irradiance Sensor
```
curl -X GET http://localhost:1020/api/v1/evaluate?device=sensor&stack=false
```
-  Read data from Weather Station
```
curl -X GET http://localhost:1020/api/v1/evaluate?device=weatherStation&stack=false
```
-  Read data from OpenMeteo API
```
curl -X GET http://localhost:1020/api/v1/evaluate?device=openmeteo&stack=false
```
If the agent runs successfully, you should see a returned Object that is similar to the one shown below.
```
{"AC Power(W)":7.922936051747742,"DC Power(W)":9.510628810971978,"timestamp":"Thu, 24 Nov 2022 09:54:51 GMT"}
```

####  [Option 2] As a stacked docker container

Running this agent in a docker stack is a more advanced option as it facilitate interactions between other agents for deployment and visualization. The stack is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

A successful setup will result in 9 containers (optional 10):
- Default containers
   - Stack Manager (exits when spins up all other containers)
   - Blazegraph
   - Nginx
   - Postgis
   - Adminer
   - Ontop
   - Gdal
   - Geoserver
- PVLibAgent
- FeatureInfoAgent (Optional)
  Note: The FeatureInfoAgent is optional and is only required if you want to visualize the result via DTVF.

##### Build the image
First, build image with:
```
docker build -t pv_lib_agent:1.1.0 .
```
The Dockerfile will automatically copy all properties files and mapping folder and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.


##### Add Config to Stack Manager
Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `./PVLibAgent/stack-manager-config/pvlib-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "pvlib-agent",
      // Other agents you wish to spin up...
    ],
    "excludes": [
      // ...
    ]
  }
}
```

After this step, the stack-manager/inputs/config folder will have the following structure:
```
config/
|_ services/
   |_ pvlib-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

##### Spin Up Stack
Follow the [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack.

##### Run the agent
Select from one of the following to read weather data:
- Read data from Irradiance Sensor
```
curl -X GET http://localhost:3838/pvlib-agent/api/v1/evaluate?device=sensor&stack=true
```
-  Read data from Weather Station
```
curl -X GET http://localhost:3838/pvlib-agent/api/v1/evaluate?device=weatherStation&stack=true
```
-  Read data from OpenMeteo API
```
curl -X GET http://localhost:3838/pvlib-agent/api/v1/evaluate?device=openmeteo&stack=true
```

If the agent runs successfully, you should see a returned Object that is similar to the one shown below.
```
{"AC Power(W)":7.922936051747742,"DC Power(W)":9.510628810971978,"timestamp":"Thu, 24 Nov 2022 09:54:51 GMT"}
```

## 4. Dockerised agent tests

The dockerised tests use one Docker container to initialise the agent and run pytest, and spin up the required Blazegraph and Postgres instances within the same stack.
To run the dockerised tests, please follow the steps below:

1. Build the docker image with the multi-stage dockerfile with the following command:
```
docker build -t pv_lib_agent:1.1.0 .
```
This command will output two images, `pv_lib_agent:1.1.0` and `pv_lib_agent_test:1.1.0`. 
The `pv_lib_agent_test:1.1.0` image will be used to run the tests.
The Dockerfile will automatically copy all properties files and mapping folder 
and set environment variables pointing to their location for testing. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.

2. Spin up the `pv_lib_agent_test:1.1.0` image and the required Blazegraph and Postgres test container instances with the following command:
```
docker compose -f "docker-compose-test.yml" up -d --build
```
