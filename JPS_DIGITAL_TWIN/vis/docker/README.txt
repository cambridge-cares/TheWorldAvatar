# Digital Twin Visualisation

The folder contains the required files to build a Docker image for the combined Digital Twin visualisation. The "Dockerfile" file contains the instructions to build the image; before making any changes to it, please consult the application's developer, Michael Hillman (mdhillman<@>cmclinnovations.com).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
* The `docker build` command should be run from the `vis` directory (not this one); this is so that a copy of the `src` directory can be copied into the image.
* A version number has been applied to this code base; this is listed in the 'version' file and should be used when tagging Images (see the overall README in the '/Deploy' directory for more details).
	
## Building the Image

Once the requirements have been addressed, the Image can be build using the following methods. Note that once this visualisation has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the visualisation (which is listed within the 'version' file).

+ To build the image:
  + `docker build --rm --no-cache -t docker.cmclinnovations.com/digital-twin:VERSION -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d --restart always --name "digital-twin" -it docker.cmclinnovations.com/digital-twin:VERSION`
+ To push the image to the CMCL registry (after logging in):
  + `docker image push docker.cmclinnovations.com/digital-twin:VERSION`