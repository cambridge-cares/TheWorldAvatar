# UK Power Grid Digital Twin Visualisation

## Purpose

This visualisation has been developed to display the status of the UK Power Grid, using the Digital Twin Visualisation Framework (TWA-VF).

## Building the Image

The `docker` folder contains the required files to build a Docker Image for the UK Power Grid visualisation; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Oisín Peppard <opeppard@cmcl.io>).

Please note the caveats below before attempting to build the service using Docker:

* The UK Power Grid visualisation installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
* A valid MapBox API must be provided in your "index" file (see the "Visualisation Framework" wiki page for more).
* A connection to the internet is required to contact remote resources and use the mapping libraries.

### Docker Commands

Once the requirements have been addressed, the Image can be built using the following methods, although it is better to pull the image from the Cambridge Cares docker registry. This is done with:

`docker pull ghcr.io/cambridge-cares/`

* To build the Image:
  * `docker-compose -f ./docker/docker-compose.yml build --force-rm`
* 
* To start a Container (i.e. run the Image):
  * `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`

The visualisation should now be available at `http://localhost:65082`
