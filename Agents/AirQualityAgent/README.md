# Description

The `AirQuality` agent is an input and output agent which queries data from the UK-AIR Sensor Observation Service ([UK-AIR]), and instantiates it according to the [OntoEMS] ontology in the [TheWorldAvatar] knowledge graph.

# Installation
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Requirements

- You need Python >3.7 to run the `AirQuality` agent. You can install Python by going to the official Python [download page]
- You also need to install a [Java Runtime Environment version >=8]

## 1. Virtual environment setup

It is highly recommended to use a [virtual environment] for the `AirQuality` agent installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv air_venv
$ air_venv\Scripts\activate.bat
(air_venv) $
```

The above commands will create and activate the virtual environment `air_venv` in the current directory.

## 2. Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `AirQuality` directly from its repository you need to first clone the [TheWorldAvatar] project. Then simply navigate to the *TheWorldAvatar\Agents\AirQualityAgent* directory and execute the following commands:
```bash
# build and install
(air_venv) $ python -m pip install .

# or build for in-place development
(air_venv) $ python -m pip install -e .
(air_venv) $ python -m pip install -r dev_requirements.txt
```

To install proper agent logging, one also needs to execute (Please note that all `agentlogging` commands are currently commented out due to performance issues):
```bash
(air_venv) $ python -m pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"
```

Alternatively, use the provided `install_script_pip.sh` convenience script, that can create the virtual environment and install the `AirQuality` agent in one go:
```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```
Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. 

## Notes on testing

Please note that some of the tests use the `testcontainers` library and, hence, require Docker to be installed. Furthermore, access to the `docker.cmclinnovations.com registry` is required from the machine the test is run on to pull docker images. You can request login details by emailing `support<at>cmclinnovations.com` with the subject 'Docker registry access'.
Furthermore, there are two integration tests which require a (local) Blazegraph and PostgreSQL RDB reachable at the endpoints specified in the [properties file]. Those tests are ignored by default and need to be actively activated if needed.

To test the code, simply run the following commands:

```bash
# Run all tests
(air_venv) $ pytest

# Run selected tests, e.g. test_datainstantiation.py
(air_venv) $ pytest test_datainstantiation.py
```

# How to use the Agent

The `AirQuality` agent can be deployed as locally running web agent or using the provided dockerized version.

## Prerequisites

Before starting the Flask web app or building the Docker image, several key properties need to be set in the [properties file]. Credentials and endpoints are needed for the TimeSeries client to access the knowledge graph and the Postgres database:
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph


## Web agent usage

In order to deploy the `AirQuality` as a web agent, simply start a server with the following app entry point:

`(Windows)`
```cmd
(air_venv) $ set FLASK_APP=airquality\flaskapp\wsgi.py & flask run
```

## Dockerized agent usage

The provided `docker-compose` file contains instructions to create Docker images for both the Debugging and Production stage. The debugging image allows for hot-reloading code changes by mounting the `airquality` folder containing the source code as external volume.

```bash
# Build debugging image and spin up container
docker-compose -f "docker-compose.yml" up -d --build airquality_agent_debug

# Build production image and spin up container
docker-compose -f "docker-compose.yml" up -d --build airquality_agent_production
```

**Please note:**  The End of Line Sequence of the `app_entry_point.sh` file might potentially need to be changed to *LF* for the Docker images to build successfully.

While the production image starts the agent immediately after the container has started, the debugging image awaits for the external debugger to connect before starting the agent. Using `VS Code`, this can be achieved by using the `launch.json` settings below:

```
    {
        "name": "Python: Remote Attach",
        "type": "python",
        "request": "attach",
        "connect": {
            "host": "127.0.0.1",
            "port": 5678
        },
        "pathMappings": [
            {
                "localRoot": "${workspaceFolder}/airquality/flaskapp/",
                "remoteRoot": "/app/airquality/flaskapp/"
            }
        ]
    }
```

A database connection issue has been observed when using the dockerised agent with locally running Postgres RDB. Therefore, a `docker-compose_stack.yml` file is provided to spin up a stack with a Blazegraph and a PostgreSQL within the same network as the agent container. For the agent to access the Blazegraph, the hostname is `blazegraph` (specified in the compose file), port number = 9999. The `sparql.query.endpoint` and `sparql.query.endpoint` to enter in the `airquality.properties` will be in the form of `http://blazegraph:9999/blazegraph/namespace/[NAME OF NAMESPACE]/sparql`. The Blazegraph namespace must have geospatial enabled. The hostname for the PostgreSQL container is `postgres`, accessible via the default port 5432. The field to enter for `db.url` will be in the form `jdbc:postgresql://postgres/[NAME OF DATABASE]`. 

**Both the Blazegraph namespace and the PostgreSQL database need to be (manually) created after spinning up the Docker stack, but before sending the first update request to the dockerised agent.** For Blazegraph, simply open the Blazegraph workbench `http://localhost:<port number from docker-compose_stack>/blazegraph` in any browser and create the needed namespace. For postgreSQL, pgAdmin can be used to connect to the database within Docker by adding a new server with `localhost` and `port number` as defined in the `docker-compose_stack` file. The new database can be created afterwards.

Both PostgreSQL and Blazegraph use volumes to ensure data persistence and the respective data can be found under `\\wsl$\docker-desktop-data\version-pack-data\community\docker` in the local file system (Windows).

```bash
# Build production image and spin up container stack
docker-compose -f "docker-compose_stack.yml" up -d --build
```


## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5000/]. All requests are to be sent as GET requests and all available endpoints are listed below:

- GET request to instantiate all UK-AIR stations (only new stations will be added, already instantiated stations will not be overwritten)
> `/api/airqualityagent/instantiate/stations` 
- GET request to instantiate UK-AIR readings for instantiated stations (only new station readings will be added, already instantiated readings will not be overwritten)
> `/api/airqualityagent/instantiate/readings`
- GET request to add latest time series readings for all instantiated time series 
> `/api/airqualityagent/update/timeseries`
- GET request to update all UK-AIR stations and associated readings, and add latest data for all time series (i.e. instantiate missing stations and readings and append latest time series readings)
> `/api/airqualityagent/update/all`
- GET request to retrieve data about UK-AIR stations and create respective output files for DTVF (i.e. request expects all individual query parameter to be provided in a single nested JSON object with key 'query')
> `/api/airqualityagent/retrieve/all`

Example requests are provided in the [resources] folder. The [example retrieve all request] contains further information about allowed parameters to query station and readings data from the knowledge graph and create the respective output files. It has to be noted that using the `circleCenter` and `circleRadius` parameters to retrieve only stations within a particular area (using Blazegraph's geospatial search capabilities) requires a Blazegraph namespace with geospatial capabilities enabled.

Agent start-up will automatically register recurring tasks to assimilate latest time series data (i.e. every hour) and to create DTVF output files (i.e. once per day). Besides those recurring background tasks, additional HTTP requests can be sent (but they might be delayed) to the agent.

# Authors #
Markus Hofmeister (mh807@cam.ac.uk), March 2022


<!-- Links -->
[UK-AIR]: https://uk-air.defra.gov.uk/data/about_sos
[OntoEMS]: http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl
[download page]: https://www.python.org/getit/
[Java Runtime Environment version >=8]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[properties file]: resources\airquality.properties
[http://localhost:5000/]: http://localhost:5000/
[resources]: resources
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http