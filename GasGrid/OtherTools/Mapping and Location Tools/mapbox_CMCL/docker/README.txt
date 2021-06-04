# Gas Grid Visualisation

The folder contains the required files to build a Docker image for the UK Gas Grid visualisation of the Digital Twin project. The "Dockerfile" file contains the instructions to build a Digital Twin image; before making any changes to it, please consult the application's developer (Tom Savage <trs53@cam.ac.uk>) and the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
* The `docker build` command should be run from the `mapbox_CMCL` directory (not this one); this is so that a copy of the `mapbox_CMCL` directory can be copied into the image.
* As part of the current git hash is used to tag the image, please do not attempt to push any images to the CMCL registry unless all pending changes are committed to a branch. If you don't then previous versions of the image in the registry may accidently be overwritten.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.
* To allow Maven to download the GEO-JSON data files from the CMCL server, you'll need to do to following before running any Docker build commands:
	
1. Make a copy of the `settings-template.xml` and name it `settings.xml`.
2. Add your Nexus username and password to the `settings.xml` file.
	* Contact CMCL administrators for a username and password if you don't already have one.
	
	
## Building the Image

Once the requirements have been addressed, the Image can be build using the following methods. Note that once this visualisation has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the visualisation (which is listed within the 'version' file).

+ To build the image:
  + `docker build --rm --no-cache -t docker.cmclinnovations.com/gas-grid-vis:VERSION -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d -p 4001:80 --restart always --name "gas-grid-vis" -it docker.cmclinnovations.com/gas-grid-vis:VERSION`
+ To push the image to the CMCL registry:
  + `docker image push docker.cmclinnovations.com/gas-grid-vis:VERSION`
