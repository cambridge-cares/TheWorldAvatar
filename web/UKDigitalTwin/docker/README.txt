The folder contains the required files to build a Docker image for the UK Power System visualisation
of the Digital Twin project. The "Dockerfile" file contains the instructions to build a Digital Twin
image; before making any changes to it, please consult the application's developer
(Wanni Xie <wx243@cam.ac.uk>) and the system administrators at CMCL
(Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

At CMCL, this tool is referred to as the "UK Power System" to differentiate it from other
similar visualisations. More information can be found on the CMCL wiki (contact CMCL for details).

Please note the caveats below before attempting to build the service using Docker:

	- The service installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from the UKDigitalTwin directory (not this one); 
	this is so that a copy of the UKDigitalTwin directory can be copied into the image.
	
	- As part of the current git hash is used to tag the image, please do not attempt to push any
	images to the CMCL registry unless all pending changes are committed to a branch. If you don't
	then previous versions of the image in the registry may accidently be overwritten.
	
	- You may need to change the exposed port depending on the NODE_ENV variable currently set
	in the Dockerfile. 

	
To build the image (with part of the git hash as the tag):
	docker build --rm --no-cache -t uk-power-system:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To generate a container (i.e. run the image):
	docker run -d -p 3001:3001 --restart always --name "uk-power-system" -it uk-power-system:`git rev-parse --short HEAD`
	
To push the image to the CMCL registry:
	# Tag the image with the registry location
	docker image tag uk-power-system:`git rev-parse --short HEAD` docker.cmclinnovations.com/uk-power-system:`git rev-parse --short HEAD`
	
	# Push the image to the registry
	docker image push docker.cmclinnovations.com/uk-power-system:`git rev-parse --short HEAD`
	
	
Once running, you should be able to access the tool at localhost:3001/ontotwinuk