# NTUPVLib Agent

The NTUPVLib Agent is a modified version of the [PVLibAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PVLibAgent) developed to work with the PV data available for the NTU use case. The agent assumes a standard PV for the purpose of running PVLib and then scales the output by the PV area for the building. An additional scale factor is included to scale the data to a magnitude appropriate for the 15-bus NTU test system.

This agent is designed to calculate AC and DC Power output from Photovaltaic Panels based on values provided in the properties files or values queried from the knowledge graph. It will then initialise the AC and DC Power as timeseries in the knowledge graph. The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) from the JPS_BASE_LIB to interact with both the knowledge graph and database and uses [PvLib](https://pvlib-python.readthedocs.io/en/stable/) for it's AC and DC Power calculations.

## 1. Property files

For the agent to read data, four property files are required:
- One [property file for DC and AC Power instantiation](#dataIRIs-properties) defining the IRIs for each of the keys.
- One [property file for the time-series client](#time-series-client-properties) defining timeseries client related parameters.
- One [property file for the Solar Model](#Model-properties) defining the parameters of the Solar PV Model.
- One [misc property file for the Solar Model](#misc-properties) defining a scale factor to apply to the calculated ac and dc power.

### dataIRIs properties
This property file is used to determine whether there are IRIs already created for the instantiation of DC and AC Power
in the knowledge graph. This property file can be left unchanged where the agent will then create IRIs for the keys indicated
in the file (this is the default approach for the NTU test case). 
More information can be found in the property file `dataIRIs.properties` in the `resources` folder.

### Time-series client properties
For deployment in stack, the endpoints must be configured in `PVLibAgent/stack_utils/stack_configs.py`

Otherwise, the time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.query.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database to query from
- `db.query.user` the username to access the Postgres database to query from
- `db.query.password` the password to access the Postgres database to query from
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `db.update.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database to update to
- `db.update.user` the username to access the Postgres database to update to
- `db.update.password` the password to access the Postgres database to update to
- `air.temperature.iri` the timeseries data IRI for air temperature
- `wind.speed.iri` the timeseries data IRI for wind speed
- `irradiance.iri` the timeseries data IRI for irradiance
- `iri` the IRI of the sensor or weather station entity that has rdf:type s3n:SmartSensor or rdf:type ontoems:ReportingStation

More information can be found in the property file `ts_client.properties` in the `resources` folder.

### Solar Model Data Preparation
#### [Option 1] Read photovoltaic model specs from property files

This is the default option for the NTU test case.

##### Model properties
The model parameters properties contains the parameters required to create a solar PV Model for calculations. The file `model_parameters.properties` can be found in the `resources` folder. It should contain the following keys:
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
Alternatively, the parameters required to create a solar PV Model (see [model properties](#Model-properties) aboce) can be read from a knowledge graph. This requires an instantiation agent to create a Knowledge Graph filled with PV model parameter values. The  [HistoricalNTUEnergy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HistoricalNTUEnergyAgent) provides an example to instantiate a knowledge graph which includes PV model parameters.

Note: currently the agent is only able to retrieve the latitude and longitude (discussed [below](#extra-longitude-and-latitude-data)).

### Misc properties

The agent takes a scale factor that can be applied to the calculated power. The scale_factor can be set in the file `misc_parameters.properties` found in the `resources` folder.

## 2. Weather Data Preparation
The [PVLibAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PVLibAgent) is designed to work with data from one of three sources: weather stations, irradiance sensors, and the OpenMeteo API. 
Only the OpenMeteo API is used for the NTUPVLibAgent.

#### Read data from OpenMeteo API
In the event that the weather data is retrieved from the OpenMeteo API, the related timeseries data has to be instantiated in the knowledge graph with the following structure:
```
<http://test_openmeteo> rdf:type ontoems:ReportingStation .
<http://test_openmeteo> ontoems:reports <http://test_parameter> .
<http://test_parameter> rdf:type ontoems:AirTemperature .
<http://test_parameter> om:hasValue <air temperature data IRI> .
```
The [OpenMeteo Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OpenMeteoAgent) provides an example of the instantiation.

The PVLib Agent will query for the latest air temperature, wind speed and global horizontal irradiance values from the knowledge graph.

#### [Extra] Longitude and latitude data
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
In the events that latitude and longitude are not instantiated in the knowledge graph with the structure above, they have to be included in the `model_parameters.properties` file. See [property file for the Solar Model](#Model-properties).

## 3. Build and Run
The PVLib agent can run either as a standalone docker container or within a stack. The build and setup procedures are different for different running modes.

NTUPVLib is intended for deployment in a stack (option 2). For others, refer to the PVLibAgent.

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
docker build -t ntu-pvlib-agent:1.0.0 .
```
The Dockerfile will automatically copy all properties files and mapping folder and set environment variables pointing to their location. Therefore, you do not need to shift the properties files and mapping folder nor add in environment variables manually.


##### Add Config to Stack Manager
Before running the stack manager, you need to add the config files to the stack manager. The config files are located in `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/`.
- Copy `stack-manager-config/ntupvlib-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.
- Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it does not exist, following the below structure.
```json
{
  "services": {
    "includes": [
      "ntupvlib-agent",
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
   |_ ntupvlib-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

##### Spin Up Stack
Follow the [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack.

##### Run the agent

Send a GET request, with the parameters including the start and end dates and times in ISO format (UTC):
- stack=true
- device=openmeteo
- start=2024-03-20T16:00:00Z
- end=2024-03-21T15:00:00Z

```
curl -X GET http://localhost:3838/pvlib-agent/api/v1/evaluate?device=openmeteo&stack=true&start=2024-03-20T16:00:00Z&end=2024-03-21T15:00:00Z
```

If the agent runs successfully, you should see a returned string
```
Success!
```