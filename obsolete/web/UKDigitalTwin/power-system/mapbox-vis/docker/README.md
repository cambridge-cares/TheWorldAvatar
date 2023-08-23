# Power System Visualisation

The folder contains the required files to build a Docker image for the MapBox version of the Power System visualisation of the Digital Twin project. The "Dockerfile" file contains the instructions to build a Power System Visualisation image; before making any changes to it, please consult the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
* The `docker build` command should be run from the `mapbox-vis` directory (not this one); this is so that a copy of the `mapbox-vis` directory can be copied into the image.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.
* Ensure that the version listed in the VERSION file is correct, and that the same version is used when tagging images. See the main README in the `Deploy` directory for more details on versioning.
	
	
## Building the Image

Once the requirements have been addressed, the Image can be build using the following methods. Note that once this visualisation has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the visualisation (which is listed within the 'version' file). The image also contains 'development' and 'production' stages, refer to the Dockerfile for more details on the difference between these.

Note that during development, you can also create a "bind mount", mounting a directory on your host OS into the Docker container, so that you don't need to rebuild it each time you change a file. This can be done using the -v argument (-v "$(pwd)":/var/www/html).

+ To build the image:
  + `docker build --rm --no-cache --target development -t docker.cmclinnovations.com/power-system-vis:1.0.0-SNAPSHOT-dev -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d -p 3001:80 --restart always --name "power-system-vis" -it docker.cmclinnovations.com/power-system-vis:1.0.0-SNAPSHOT-dev`
+ To push the image to the CMCL registry (after logging in):
  + `docker image push docker.cmclinnovations.com/power-system-vis:1.0.0-SNAPSHOT-dev`