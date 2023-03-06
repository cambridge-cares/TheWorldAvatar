Prerequisites
1) Set Mapbox user and API key in DispersionVis/index.html
2) Ship data needs to be present in ShipInputAgent/data
3) Set openweather API key in ../Agents/WeatherAgent, more details in that folder

Stack needs to be up and running:
1) Navigate to Deploy/stacks/dynamic/stack-manager, do all required setup as described by the README and execute (in a WSL terminal)
```
./stack.sh start ship-stack
```

2) execute
```
./startup.sh
```

in this folder to spin up other required containers.

- Initialise scope by:
```
curl -X POST "http://localhost:8084/DispersionInteractor/InitialiseSimulation?ewkt=SRID=4326;POLYGON((-4.282264034358564%2050.26375198971232,-4.001705368451314%2050.26650880607838,-4.005497340234552%2050.44635115729881,-4.287117430213462%2050.44357678715814,-4.282264034358564%2050.26375198971232))&nx=400&ny=400"
```

This request should return the IRI of the derivation, record this. Check the README of DispersionInteractor for more details.

- Trigger ship input agent and update simulation time (input to dispersion derivation)
```
curl -X POST http://localhost:8084/DispersionInteractor/UpdateShipsAndSimulationTime
```

- Finally trigger update for the dispersion derivation
```
curl -X POST "http://localhost:8084/DispersionInteractor/TriggerUpdateDispersion?derivation=PLACE_DERIVATION_IRI_HERE"
```

- Visualisation can be accessed on the browser at
```
http://localhost:8090
```