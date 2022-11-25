# Description

This repository contains several (helper) scripts to support the finalisation of a MVP of the King'sLynn use case required for the Derivation Paper. This is **NOT** an agent; however, multiple parts of the scripts developed here will be migrated into agents actually used in the (not simplified/mocked) King's Lynn use case.

The details provided here are mainly for documentation of the overall workflow to enable the use case MVP and ensure reproducibility.


# Requirements

- The following scripts have been tested with Python >3.9
- To use `py4jps` one also needs [Java 11] installed
- As this instantiation uses Blazegraph and PostgreSQL running in Docker containers, you need to have Docker installed on your machine. Details on how to set up a [Docker environment] can be found in the TWA wiki. Furthermore, access to the [CMCL Docker image registry] is required.


## Installation of required packages

It is highly recommended to use a [virtual environment], which can be created as follows:

`(Windows)`
```cmd
$ python -m venv deriv_venv
$ deriv_venv\Scripts\activate.bat
(deriv_venv) $
```

To install required packages, run the following command:

```bash
# build and install
(deriv_venv) $ python -m pip install -r dev_requirements.txt
```

## Spinning up Docker Stack

A [docker-compose_stack.yml] file is provided to spin up a stack with a Blazegraph and a PostgreSQL container. Both PostgreSQL and Blazegraph use volumes to ensure data persistence. To spin up the stack, run the following command from the same directory where this README is located:
```bash
# Spin up container stack
docker-compose -f "docker-compose_stack.yml" up -d
```

<!--
## Prerequisites

Before starting the Flask web app or building the Docker image, several key properties need to be set in the [properties file]. Credentials and endpoints are needed for the TimeSeries client to access the knowledge graph and the Postgres database:
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph


A database connection issue has been observed when using the dockerised agent with locally running Postgres RDB. Therefore, a `docker-compose_stack.yml` file is provided to spin up a stack with a Blazegraph and a PostgreSQL within the same network as the agent container. For the agent to access the Blazegraph, the hostname is `blazegraph` (specified in the compose file), port number = 9999. The `sparql.query.endpoint` and `sparql.query.endpoint` to enter in the `airquality.properties` will be in the form of `http://blazegraph:9999/blazegraph/namespace/[NAME OF NAMESPACE]/sparql`. The Blazegraph namespace must have geospatial enabled. The hostname for the PostgreSQL container is `postgres`, accessible via the default port 5432. The field to enter for `db.url` will be in the form `jdbc:postgresql://postgres/[NAME OF DATABASE]`. 

**Both the Blazegraph namespace and the PostgreSQL database need to be (manually) created after spinning up the Docker stack, but before sending the first update request to the dockerised agent.** For Blazegraph, simply open the Blazegraph workbench `http://localhost:<port number from docker-compose_stack>/blazegraph` in any browser and create the needed namespace. For postgreSQL, pgAdmin can be used to connect to the database within Docker by adding a new server with `localhost` and `port number` as defined in the `docker-compose_stack` file. The new database can be created afterwards.

Both PostgreSQL and Blazegraph use volumes to ensure data persistence and the respective data can be found under `\\wsl$\docker-desktop-data\version-pack-data\community\docker` in the local file system (Windows).

```bash
# Build production image and spin up container stack
docker-compose -f "docker-compose_stack.yml" up -d --build
```
-->


# Authors #
Markus Hofmeister (mh807@cam.ac.uk), November 2022


<!-- Links -->
[Java 11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[Docker environment]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment
[CMCL Docker image registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry