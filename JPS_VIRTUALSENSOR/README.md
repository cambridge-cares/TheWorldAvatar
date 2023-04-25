## Prerequisites

1) Set Mapbox user and API key in `./DispersionVis/index.html`
2) Ship data needs to be present in `./ShipInputAgent/data`
3) Set openweather API key in `./stack-manager/inputs/config/services/weather-agent.json`
4) Create two files called `postgis_password` and `geoserver_password` in the `./stack-manager/inputs/secrets/` directory. Populate the files with the intended passwords for PostGIS and GeoServer, respectively.

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
http://localhost:3838/dispersion-vis/
```

To see results at a later simulation time, repeat steps 2-4.

## Debugging in VSCode

1) Create a copy of the JSON file of a container for debugging in `./stack-manager/inputs/config/services/`
2) Add the following to environment variable specification (`Env`):

```
"JPDA_ADDRESS=0.0.0.0:5005",
"JPDA_TRANSPORT=dt_socket",
"DEBUG=ON"
```

3) Add the following as the child of `ServiceSpec`:

```
"EndpointSpec": {
	"Ports": [
		{
			"Name": "web",
			"Protocol": "tcp",
			"TargetPort": "5005",
			"PublishedPort": "5005"
		}
	]
}
```

4) Update `ship-stack.json` accordingly.

5) In the folder of the corresponding container, change the port of Reattach and Debug in `.vscode/launch.json` (`${input:debug.port.read}`) to match the `PublishedPort`.

6) After starting the stack, run Reattach and Debug in VSCode.

`./stack-manager/inputs/config/services/aermod-agent-debug.json` is included as an example.