The folder contains the required files to build a Docker image for the Uk Digital Twin. The "Dockerfile"
file contains the instructions to build a Digital Twin image; before making any changes to it, please
consult the application's developer (Wanni Xie <wx243@cam.ac.uk>) and the system administrators
at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).


Please note the caveats below before continuing:

	- The digital twin installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from the UKDigitalTwin directory (not this one); 
	this is so that a copy of the UKDigitalTwin directory can be copied into the image.
	

To build the image:
	docker build --rm --no-cache -t uk-digital-twin:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To run the image and generate a container:
	docker run -d -p 3001:3001 --restart always --name "uk-digital-twin" -it uk-digital-twin:`git rev-parse --short HEAD`
	
	
Once running, you should be able to access the tool at localhost:3001/ontotwinuk (note that the port may change depending
on the NODE_ENV variable).
