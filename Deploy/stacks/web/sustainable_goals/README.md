# UN Sustainable Goals Visualisation

The folder contains the required files to build a Docker image for the UN Sustainable Goals visualisation of the Digital Twin project. The "Dockerfile" file contains the instructions to build a Docker image; before making any changes to it, please consult the application's developer(s) or the system administrators at CMCL (Michael Hillman mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

	- The service installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from the sustainable_goals directory (not this one); 
	this is so that a copy of the sustainable_goals directory can be copied into the image.
	
	- As part of the current git hash is used to tag the image, please do not attempt to push any
	images to the CMCL registry unless all pending changes are committed to a branch. 
	
	- The port shown below has been set so that it doesn't collide with any other services running
	on the CMCL systems, feel free to change it temporarily for local testing/development.
	
	- You may need to change the MapBox API token within the index.html file before building. The API
	token used by CMCL will only function when hosted on/used from the kg.cmclinnovations.com website.
	
	
The Image can be build as part of the web stack using the scripts found in the Deploy/stacks directory. However, to build the Image in isolation, use the commands listed below.

To build the image (with part of the git hash as the tag):
	docker build --rm --no-cache -t un-goals:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To generate a container (i.e. run the image):
	docker run -d -p 4002:80 --restart always --name "un-goals" -it un-goals:`git rev-parse --short HEAD`
	
To push the image to the CMCL registry:
	docker image tag un-goals:`git rev-parse --short HEAD` docker.cmclinnovations.com/un-goals:`git rev-parse --short HEAD`
	docker image push docker.cmclinnovations.com/un-goals:`git rev-parse --short HEAD`
