# Example Digital Twin Visualisation

## Building the Image
The `docker` folder contains the required files to build a Docker Image for the example visualisation; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The example visualisation within the Docker image will be based on the current content of the `queried_data` repository at the point of building the image.
* A valid MapBox API key (can be obtained for free by signing up) must be provided in your `overall-meta.json` file (see the [DTVF] wiki page for more information).
* A connection to the internet is required to contact remote resources and use the mapping libraries.

### Docker Commands
Once the requirements have been addressed, the Image can be built using the following commands (to be run in CMD from within the `dtvf_visualisation` repository):

+ To build the Image:
  + `docker-compose -f ./docker/docker-compose.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`

Afterwards the visualisation can be viewed via the `Open in Browser` button in Docker Desktop.

[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations

