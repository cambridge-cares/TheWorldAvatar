# King's Lynn Agent

The target of this agent is to provide functionality for an interoperable city environment, combining static built environment and dynamic (real-time) time series data. The exact scope and focus will change during development.

Please note, that this project is still **highly work in progress**!

## Preparation
### 1. Installing the project (on Windows)

1) Open `cmd` terminal and navigate into project's root repository (referred to as `<root>` in the following)

2) Running the following command will create a virtual environment, install all required packages stated in the `dev_requirements.txt` file, and install the project itself in developer mode:
```
install_script_pip.sh -v -i -e
```

Please note: The `install_script_pip.sh` only works on Windows OS. For usage on Mac, please create virtual environment, install requirements and the project itself manually.

### 2. Instantiate buildings

This project does not contain any functions to instantiate buildings in the KG. Instead, the [CitiesKG Agents] can be used to instantiate the static built environment (i.e. buildings). 

## Usage
### 1. Query buildings

Before querying any buildings from the KG, please make sure that the [properties file] is correctly populated with the respective SPARQL endpoints and output directory, as well as your Mapbox API key in case the Digital Twin Visualisation Framework (DTVF) shall be used for later visualisation.

Before querying buildings, please specify the number of buildings to retrieve, the required output dimension, and the target coordinate reference system in the `query_buildings.py` file. Afterwards, simply execute the `query_buildings.py` script to retrieve the buildings and write the output to GeoJSON and JSON files. 

### 2. Visualise buildings using the DTVF
After the buildings have successfully been queried, the DTVF Image can be built using the following commands (to be run in CMD from within the `<root>\dtvf_visualisation` repository and with **deactivated** virtual environment):

To build the Image:
```
docker-compose -f ./docker/docker-compose.yml build --force-rm
```
To generate a Container (i.e. run the Image):
```
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate
```

Afterwards the visualisation can be viewed via Docker Desktop. For more information, please see the [DTVF] wiki page or read the detailed `readme` file of the corresponding [Minimum Working Example].


[properties file]: resources/properties.properties
[CitiesKG Agents]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[Minimum Working Example]: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/TimeSeriesExample
