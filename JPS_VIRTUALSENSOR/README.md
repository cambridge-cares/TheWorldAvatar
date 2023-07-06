Prerequisites
1) Make a copy of DispersionVis/indexTemplate.html and set its file name to be 'index.html'. Set Mapbox user and API key in DispersionVis/index.html
2) Ship data needs to be present in ShipInputAgent/data. If the agent is being run for chemical plants instead of ships, 
it is still necessary to define one ship in a .json file in this folder. In this case, the ship should be placed outside the region for which AERMOD will be run, which is specified in WKT format in the POST request to the DispersionInteractor class. The longitude of each coordinate must be specified before the latitude.  

3) Set openweather API key in stack-manager/inputs/config/services/weather-agent.json, see ../Agents/WeatherAgent folder for more details

4) If running AERMOD for static point sources, it is necessary to instantiate the input data required for AERMOD Agent according to OntoDispersion (https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodispersion). See the JurongIslandInputAgent folder for an example of an agent that does this.

5) If running AERMOD for static point sources, it is required to upload the elevation data to a single table in the stack postgresql database. The elevation data for the user-specified region will be queried from this table and used to run the AERMOD terrain pre-processor, AERMAP, which calculates the critical hill height scale for each receptor. 

Elevation data can be downloaded from https://www.eorc.jaxa.jp/ALOS/en/dataset/aw3d30/aw3d30_e.htm as a series of .tif files each of which covers a region that spans 1 degree in latitude and longitude. It is required to register for a free account before downloading the files.

Uploading of elevation data can be done using the stack data uploader. The downloaded .tif files should be placed in a subfolder within the TheWorldAvatar/JPS_VIRTUALSENSOR/stack-data-uploader/inputs/data/elevation/ directory. The elevation.json configuration file in the TheWorldAvatar/JPS_VIRTUALSENSOR/stack-data-uploader/inputs/config/ can be modified if necessary. It is also possible to include an elevation.sld file in this directory location to customize the GeoServer style used to upload the data. See https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader for more details. The name of the POSTGIS table where the elevation data is stored needs to be specified as the value of the environment variable "ELEVATION_TABLE" in the stack-manager/inputs/config/aermod-agent.json and aermod-agent-debug.json files. 



Stack needs to be up and running:
1) execute
```
./startup.sh
```

in this folder to spin up pull docker images, spin up containers (using stack manager) and copy files into container volumes.

Make sure you have access to the CMCL Docker registry. You can test your access by runing 
    ```console
    docker login docker.cmclinnovations.com
    ```
If you are not already logged in then, when prompted, enter the username and password you were given.

2) If running AERMOD for static point sources, save the json configuration file and .tif elevation data files in the TheWorldAvatar/JPS_VIRTUALSENSOR/stack-data-uploader/inputs/ folder as described above. Execute 

```
./stack.sh start ship-stack
```
from within this folder to upload the elevation data to POSTGIS.

3) Start the docker containers for the input agents responsible for instantiating emissions data. Send the requests to these agents to instantiate the relevant triples.

## Work example

1) Initialise scope by:
```
curl -X POST "http://localhost:3838/dispersion-interactor/InitialiseSimulation?ewkt=SRID=4326;POLYGON((-4.282264034358564%2050.26375198971232,-4.001705368451314%2050.26650880607838,-4.005497340234552%2050.44635115729881,-4.287117430213462%2050.44357678715814,-4.282264034358564%2050.26375198971232))&nx=400&ny=400"
```

This request should return the IRI of the derivation, record this. Check the README of DispersionInteractor for more details.

By providing an optional parameter, "citiesnamespace", AermodAgent will query buildings data from the provided namespace listed in http://www.theworldavatar.com:83/citieskg/#namespaces, e.g.
```
curl -X POST "http://localhost:3838/dispersion-interactor/InitialiseSimulation?ewkt=SRID=4326;POLYGON((7.58%2049.2,7.58%2049.236,7.61%2049.236,7.61%2049.2,7.58%2049.2))&nx=30&ny=30&citiesnamespace=pirmasensEPSG32633"
```

2) Trigger ship input agent and update simulation time (input to dispersion derivation)
```
curl -X POST http://localhost:3838/dispersion-interactor/UpdateShipsAndSimulationTime
```

3) Finally trigger update for the dispersion derivation
```
curl -X POST "http://localhost:3838/dispersion-interactor/TriggerUpdateDispersion?derivation=PLACE_DERIVATION_IRI_HERE"
```

4) Visualisation can be accessed on the browser at
```
http://localhost:3838/dispersion-vis

```
The updated version of the agent also displays the legend for the contour plot in the sidebar. It may be necessary to open an incognito browser window to view it. 
