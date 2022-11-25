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
(deriv_venv) $ python -m pip install -r requirements.txt
```

## Spinning up Docker Stack

A [docker-compose_stack.yml] file is provided to spin up a stack with a Blazegraph and a PostgreSQL container. Both PostgreSQL and Blazegraph use volumes to ensure data persistence. To spin up the stack, run the following command from the same directory where this README is located:
```bash
# Spin up container stack
docker-compose -f "docker-compose_stack.yml" up -d
```

# Workflow

## 1. Consolidate previously exported triples

This minimum demonstration example is based on two previously exported sets of triples: One instantiation of properties (i.e. buildings and flats) includes their geospatial location (as points) and the other contains previous sales transactions (if available). Both files can be found on [Dropbox]. The data in both files have been consolidated by matching properties based on their identifiers, which matches for buildings with available EPC, and hence address and sales transaction data. The [consolidated triples] can also be found on Dropbox and the SPARQL `matching_query` is provided in the [resources] folder (for reference).

## 2. Instantiate consolidated triples

Before starting the instantiation, ensure that the properties in [configs.py] match the settings in the `docker-compose_stack.yml` file. Then download the [consolidated triples] file and place it into the [input_data] folder (filename to be specified in [data_preparation.py]).

Then simply run [data_preparation.py] as main script.

## 3. Identify buildings within polygon




# Authors #
Markus Hofmeister (mh807@cam.ac.uk), November 2022


<!-- Links -->
[Java 11]: https://adoptium.net/en-GB/temurin/releases/?version=11
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[Docker environment]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment
[CMCL Docker image registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

<!-- Data -->
[Dropbox]: https://www.dropbox.com/home/CoMo%20shared/mh807/DerivationPaper/data
[consolidated triples]: https://www.dropbox.com/home/CoMo%20shared/mh807/DerivationPaper/data?preview=consolidated_properties.nt
[resources]: resources
[configs.py]: configs.py
[input_data]: input_data
[data_preparation.py]: data_preparation.py