## Prerequisites
1) Create two plain text files in the DispersionVis folder - `mapbox_username` and `mapbox_api_key`, with a valid MapBox credentials in them.
2) Ship data needs to be present in ShipInputAgent/data. If the agent is being run for static point sources only, this is not required. 
3) Set openweather API key in stack-manager/inputs/config/services/weather-agent.json, see ../Agents/WeatherAgent folder for more details
4) If running AERMOD for static point sources, it is necessary to instantiate the input data required for AERMOD Agent according to OntoDispersion (https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodispersion). See the JurongIslandInputAgent folder for an example of an agent that does this.
5) Elevation data (optional):
AERMOD agent will try to query elevation data from a table named `elevation` in the default database. AERMOD agent can query the data stored in any SRID, but the table needs to contain data in one SRID only, hence it's recommended to convert any elevation data to a uniform SRID, e.g. 4326. An example is provided in `stack-data-uploader/inputs/config/elevation.json`. Note that this config file is written for data in SRID=32632 and it needs to be changed according to your source data. The raw data files should be stored in `stack-data-uploader/inputs/data/elevation`, any format supported by gdal should work, see https://gdal.org/drivers/raster/index.html for more info.
6) Buildings data (optional for ships, compulsory for static point source use cases):
An example config file is given in `stack-data-uploader/inputs/config/building-pirmasens.json`, corresponding raw data should be populated in `stack-data-uploader/inputs/data/pirmasens_final_citygml`

To start up the stack, execute
```
./startup.sh
```
in this folder to spin up pull docker images, spin up containers (using stack manager) and copy files into container volumes.

Make sure you have access to the CMCL Docker registry. You can test your access by runing 
    ```console
    docker login docker.cmclinnovations.com
    ```
If you are not already logged in then, when prompted, enter the username and password you were given.

## HTTP requests 
A number of examples are prepared in the `HTTP requests` folder. Note that you need to install the humao.rest-client extension in VS code to run these files.

## Example 
### With ships 
This workflow calls the ShipInputAgent to add 1 timestep worth of data before triggering an update for AERMOD.
1) Make sure ShipInputAgent/data is populated with data.
2) Initialise a simulation, e.g. `HTTP requests/initialisation/plymouth.http`, you should receive a response in the form of 
```
{"derivation": "http://derivation_1"}
```
record this derivation IRI.
3) To trigger an AERMOD simulation, execute `HTTP requests/trigger update/GenerateDataWithShips.http`, be sure to replace the derivation IRI in the request from the response from the previous step.

### Without ships (only static point source)
1) Initialise a simulation, e.g. `HTTP requests/initialisation/pirmasens1.http`, record derivation IRI in the response.
2) Make sure there is at least one static point source instantiated before triggering an AERMOD simulation and the representative building object is also present.
3) Execute `HTTP requests/trigger update/GenerateDataWithoutShips.http`, be sure to enter derivation IRI in the request.

## Visualisation
Visualisation can be viewed at http://localhost:3838/dispersion-vis. Note that if buildings data is present in ontop, the visualisation may take a while to load because the first query takes time. 
