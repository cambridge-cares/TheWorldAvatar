# Gas Grid Agent

The folder contains the required files to build a Docker image for the Gas Grid agent. The "Dockerfile" file contains the instructions to build an image; before making any changes to it, please consult the application's developer (Tom Savage <trs53@cam.ac.uk>) and the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
* The `docker build` command should be run from the `GasGrid` directory (not this one); this is so that a copy of the `GasGrid` directory can be copied into the image.
* As part of the current git hash is used to tag the image, please do not attempt to push any images to the CMCL registry unless all pending changes are committed to a branch. If you don't then previous versions of the image in the registry may accidently be overwritten.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.
	
## Building the Image

Once the requirements have been addressed, the Image can be build using the following methods. Note that once this code has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the software (which is listed within the 'version' file). For more information, see the readme in the '/Deploy' directory.

+ To build the image:
  + `docker build --rm --no-cache -t docker.cmclinnovations.com/gas-grid-agent:VERSION -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d -p 4005:80 --restart always --name "gas-grid-agent" -it docker.cmclinnovations.com/gas-grid-agent:VERSION`
+ To push the image to the CMCL registry (after logging in):
  + `docker image push docker.cmclinnovations.com/gas-grid-agent:VERSION`