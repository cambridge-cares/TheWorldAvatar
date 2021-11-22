# Example Digital Twin Visualisation

## Purpose
This example visualisation has been put together to demonstrate the intended use of the centralised Digital Twin Visualisation Framework (DTVF). This framework has been designed to make it easier for users not experienced with Javascript (or the mapping libraries) to quickly & easily put together a new Digital Twin visualisation. It is intended for developers to use this example visualisation to gain an understanding of the DTVF before attempting to create their own visualisation; to do that, this example can be copied and used as a starting point.

This centralised framework expects users to have structured their data in a particular format, and provide a number of associated metadata files. Before starting, ensure your data meets this format by reading the ["Visualisation Framework"](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page on the GitHub wiki. This page also details how the framework functions, and how to configure your visualisation to use it.

## Building the Image
The `docker` folder contains the required files to build a Docker Image for the example visualisation; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The example visualisation installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
* A valid MapBox API must be provided in your "index" file (see the "Visualisation Framework" wiki page for more).
* A connection to the internet is required to contact remote resources and use the mapping libraries.
* At the time of writing, the JS and CSS files comprising the framework are stored within the example visualisation (in the `js/framework` and `css/framework` directories). In the future, these files will be separated from the example, hosted remotely (perhaps on the kg.cmclinnovations.com site), then imported as remote resources in any visualisation. Until that happens, developers creating a new visualisation can take a copy of the entire `example-dt-vis` directory and use that as a base.

### Docker Commands
Once the requirements have been addressed, the Image can be built using the following methods.

+ To build the Image:
  + `docker-compose -f ./docker/docker-compose.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`
