The folder contains the required files to build a Docker image for the UK Gas Grid visualisation
of the Digital Twin project. The "Dockerfile" file contains the instructions to build a Digital Twin
image; before making any changes to it, please consult the application's developer
(Tom Savage <trs53@cam.ac.uk>) and the system administrators at CMCL
(Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

At CMCL, this tool is referred to as the "UK Gas Grid" to differentiate it from other
similar visualisations. More information can be found on the CMCL wiki (contact CMCL for details).

Please note the caveats below before attempting to build the service using Docker:

	- The service installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from the mapbox_CMCL directory (not this one); 
	this is so that a copy of the mapbox_CMCL directory can be copied into the image.
	
	- As part of the current git hash is used to tag the image, please do not attempt to push any
	images to the CMCL registry unless all pending changes are committed to a branch. If you don't
	then previous versions of the image in the registry may accidently be overwritten.
	
	- The port shown below has been set so that it doesn't collide with any other services running
	on the CMCL systems, feel free to change it temporarily for local testing/development.
	
	
To build the image (with part of the git hash as the tag):
	docker build --rm --no-cache -t uk-gas-grid:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To generate a container (i.e. run the image):
	docker run -d -p 4001:80 --restart always --name "uk-gas-grid" -it uk-gas-grid:`git rev-parse --short HEAD`
	
To push the image to the CMCL registry:
	# Tag the image with the registry location
	docker image tag uk-gas-grid:`git rev-parse --short HEAD` docker.cmclinnovations.com/uk-gas-grid:`git rev-parse --short HEAD`
	
	# Push the image to the registry
	docker image push docker.cmclinnovations.com/uk-gas-grid:`git rev-parse --short HEAD`