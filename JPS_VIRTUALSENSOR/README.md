Prerequisites
1) Make a copy of DispersionVis/indexTemplate.html and set its file name to be 'index.html'. Set Mapbox user and API key in DispersionVis/index.html
2) Ship data needs to be present in ShipInputAgent/data. If the agent is being run for chemical plants instead of ships, 
it is still necessary to define one ship in a .json file in this folder. In this case, the ship should be placed outside the region for which AERMOD will be run, which is specified in WKT format in the POST request to the DispersionInteractor class. The longitude of each coordinate must be specified before the latitude.  

    The pollutant emitting points located within chemical plants need to be instantiated in TheWorldAvatar blazegraph with an rdf:type of PlantItem. See https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontochemplant/OntoChemPlant.owl for details of the ontology. There must be at least one pollutant source (either ship or chemical plant item) located within the region of interest. 


3) Set openweather API key in ../Agents/WeatherAgent, more details in that folder

4) Set the values of the following in the AermodAgent/docker-compose.yml file: NUMBER_SOURCES, NUMBER_BUILDINGS, INCLUDE_ELEVATION. Note that setting NUMBER_BUILDINGS to a value greater than 500 may result in the buildings pre-processor,  BPIPPRM, taking a long time to complete. The terrain pre-processor, AERMAP, may also take a long time to run for large numbers of receptors. As elevation data is an optional input for AERMOD, the user has the option of not running AERMAP by specifying INCLUDE_ELEVATION=false.

5) Download the required elevation data files from https://www.eorc.jaxa.jp/ALOS/en/dataset/aw3d30/aw3d30_e.htm. It is necessary to register for a free account first. The data files required by AERMAP as input end in "_DSM.tif". Each such data file obtained from JAXA spans a region that is 1 degree by 1 degree in terms of longitude and latitude. The latitude and longitude corresponding to the corner of minimum longitude and latitude is included in the filename. For example, the file 'ALPSMLC30_N001E103_DSM.tif' contains the elevation data for all points whose longitude is between 103 and 104 degrees and whose latitude is between 1 and 2 degrees. If running AERMAP for a large region, it may be necessary to supply multiple elevation data files as input. Each data file should be placed in the directory JPS_VIRTUALSENSOR/AermodAgent/src/main/resources/. The name of each data file should be specified in a new line following the 'DATATYPE NED' line in aermap.inp as per the format 'DATAFILE NAME_DATA_FILE', where 'NAME_DATA_FILE' should be replaced by the actual filename.

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

## Work example

1) Initialise scope by:
```
curl -X POST "http://localhost:3838/dispersion-interactor/InitialiseSimulation?ewkt=SRID=4326;POLYGON((7.58%2049.2,7.58%2049.236,7.61%2049.236,7.61%2049.2,7.58%2049.2))&nx=30&ny=30&citiesnamespace=pirmasensEPSG32633""
```

This request should return the IRI of the derivation, record this. Check the README of DispersionInteractor for more details.

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
http://localhost:8090
```
The updated version of the agent also displays the legend for the contour plot in the sidebar. It may be necessary to open an incognito browser window to view it. 
