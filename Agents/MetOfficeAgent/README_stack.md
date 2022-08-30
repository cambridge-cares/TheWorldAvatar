# Description

The `MetOffice` agent is an input and output agent which queries data from the MetOffice API, also known as [DataPoint], and instantiates it according to the [OntoEMS] ontology in the [TheWorldAvatar] knowledge graph.

# Installation
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Requirements

- You need Python >3.7 to run the `MetOffice` agent
- You also need to install a [Java Runtime Environment version >=8]

## 1. Virtual environment setup

It is highly recommended to use a [virtual environment] for the `MetOffice` agent installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv metoff_venv
$ metoff_venv\Scripts\activate.bat
(metoff_venv) $
```

The above commands will create and activate the virtual environment `metoff_venv` in the current directory.

## 2. Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `MetOffice`  directly from its repository you need to first clone the [TheWorldAvatar] project. Then simply navigate to the *TheWorldAvatar\Agents\MetOfficeAgent* directory and execute the following commands:
```bash
# build and install
(metoff_venv) $ python -m pip install --upgrade pip
(metoff_venv) $ python -m pip install .
(metoff_venv) $ python -m pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"
```

Alternatively, use the provided `install_script_pip.sh` convenience scripts, that can create the virtual environment and install the `MetOffice` agent in one go:
```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
```


## 3. Updating and adding py4jps resources

After installation of all required packages (incl. `py4jps`), the `JpsBaseLib` resource might need to get updated and the `StackClients` resource needs to be added to allow for access through `py4jps`. The required steps are detailed in the [py4jps] documentation and summarized below:

- Build latest versions of [JPS_BASE_LIB] and [Stack-Clients] from the respective branches using Maven (potentially include -DksipTests flag):
    ```
    $ mvn clean package
    ```
- Copy the project main `.jar` file and the entire `lib` folder into temporary directories, e.g., `tmp_base_lib` and `tmp_stack`
- Update `JpsBaseLib` resource
    ```
    jpsrm uninstall JpsBaseLib
    jpsrm install JpsBaseLib <path to tmp_base_lib> --jar <name of .jar file>
    ```
- Install `StackClients` resource
    ```
    jpsrm install StackClients <path to tmp_stack> --jar <name of .jar file>
    ```

# Spinning up the Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start developing there. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout dev-MetOfficeAgent-withinStack
$ git pull
```

Once the repository clone is obtained, please follow these instructions to [spin up the stack] on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.


# How to use the Agent

The `MetOffice` agent can be deployed as locally running web agent or using the provided dockerized version.

## Prerequisites

Before starting the Flask web app or building the Docker image, several key properties need to be set in the [properties file]:
- `api.key` API key to retrieve data from the MetOffice DataPoint API. Requires registration for the [DataPoint] platform

Further credentials and endpoints are needed for the TimeSeries client to access the knowledge graph and the Postgres database:
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph


## Web agent usage

In order to deploy the `MetOffice` as a web agent, simply start a server with the following app entry point:

`(Windows)`
```cmd
(metoff_venv) $ set FLASK_APP=metoffice\flaskapp\wsgi.py & flask run
```

## Dockerized agent usage

The provided `docker-compose` file contains instructions to create Docker images for both the Debugging and Production stage. The debugging image allows for hot-reloading code changes by mounting the `metoffice` folder containing the source code as external volume.

```bash
# Build debugging image and spin up container
docker-compose -f "docker-compose.yml" up -d --build metoffice_agent_debug

# Build production image and spin up container
docker-compose -f "docker-compose.yml" up -d --build metoffice_agent_production
```

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
                "localRoot": "${workspaceFolder}/metoffice/flaskapp/",
                "remoteRoot": "/app/metoffice/flaskapp/"
            }
        ]
    }
```

A database connection issue has been observed when using the dockerised agent with locally running Postgres RDB. Therefore, a `docker-compose_stack.yml` file is provided to spin up a stack with a Blazegraph and a PostgreSQL within the same network as the agent container. For the agent to access the Blazegraph, the hostname is `blazegraph` (specified in the compose file), port number = 9999. The `sparql.query.endpoint` and `sparql.query.endpoint` to enter in the `metoffice.properties` will be in the form of `http://blazegraph:9999/blazegraph/namespace/[NAME OF NAMESPACE]/sparql`. The Blazegraph namespace must have geospatial enabled. The hostname for the PostgreSQL container is `postgres`, accessible via the default port 5432. The field to enter for `db.url` will be in the form `jdbc:postgresql://postgres/[NAME OF DATABASE]`.
Both the Blazegraph namespace and the PostgreSQL database need to be (manually) created after spinning up the Docker step, but before sending the first update request to the dockerised agent.

Both PostgreSQL and Blazegraph use volumes to ensure data persistence and the respective data can be found under `\\wsl$\docker-desktop-data\version-pack-data\community\docker` in the local file system (Windows) - simply paste this path into the file explorer to inspect the respective location..


## Provided functionality

An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root [http://localhost:5000/]. All requests are to be sent as GET requests and all available endpoints are listed below:

- GET request to instantiate all Met Office stations (only new stations will be added, already instantiated stations will not be overwritten)
> `/api/metofficeagent/instantiate/stations` 
- GET request to instantiate Met Office readings for instantiated stations (only new station readings will be added, already instantiated readings will not be overwritten)
> `/api/metofficeagent/instantiate/readings`
- GET request to add latest time series readings for all instantiated time series 
> `/api/metofficeagent/update/timeseries`
- GET request to update all stations and associated readings, and add latest data for all time series (i.e. instantiate missing stations and readings and append latest time series readings)
> `/api/metofficeagent/update/all`
- GET request to retrieve data about Met Office stations and create respective output files for DTVF (i.e. request expects all individual query parameter to be provided in a single nested JSON object with key 'query')
> `/api/metofficeagent/retrieve/all`

Example requests are provided in the [resources] folder. The [example retrieve all request] contains further information about allowed parameters to query station and readings data from the knowledge graph and create the respective output files. It has to be noted that using the `circleCenter` and `circleRadius` parameters to retrieve only stations within a particular area (using Blazegraph's geospatial search capabilities) requires a Blazegraph namespace with geospatial capabilities enabled.

Agent start-up will automatically register recurring tasks to assimilate latest time series data (i.e. every hour) and to create DTVF output files (i.e. once per day). Besides those recurring background tasks, additional HTTP requests can be sent (but they might be delayed) to the agent.

# Authors #
Markus Hofmeister (mh807@cam.ac.uk), March 2022

(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com))


<!-- Links -->
[DataPoint]: https://www.metoffice.gov.uk/services/data/datapoint/about
[OntoEMS]: http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl
[Java Runtime Environment version >=8]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar
[properties file]: resources\metoffice.properties
[http://localhost:5000/]: http://localhost:5000/
[resources]: resources
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http
[py4jps]: https://pypi.org/project/py4jps/#description
[JPS_BASE_LIB]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB
[Stack-Clients]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-MetOfficeAgent-withinStack/Deploy/stacks/dynamic/stack-clients
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md