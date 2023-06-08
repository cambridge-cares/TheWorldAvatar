Prerequisites
1) Make a copy of DispersionVis/indexTemplate.html and set its file name to be 'index.html'. Set Mapbox user and API key in DispersionVis/index.html
2) Ship data needs to be present in ShipInputAgent/data. If the agent is being run for chemical plants instead of ships, 
it is still necessary to define one ship in a .json file in this folder. In this case, the ship should be placed outside the region for which AERMOD will be run, which is specified in WKT format in the POST request to the DispersionInteractor class. The longitude of each coordinate must be specified before the latitude.  

3) Set openweather API key in ../Agents/WeatherAgent, more details in that folder

4) If running AERMOD for static point sources, it is necessary to instantiate the input data required for AERMOD Agent according to OntoDispersion (https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodispersion). See the JurongIslandInputAgent folder for an example of an agent that does this.


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
