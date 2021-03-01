The folder contains the required files to build a Docker image for the JPS Chatbot. The "Dockerfile"
file contains the instructions to build a JPS Chatbot image; before making any changes to it, please
consult the application's developer (Xiaochi Zhou <xz378@cam.ac.uk>) and the system administrators
at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).


Please note the caveats below before continuing:

	- The chatbot installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- The "docker build" command should be run from the JPS_Chatbot directory (not this one); 
	this is so that a copy of the JPS_Chatbot directory can be copied into the image.
	

To build the image:
	docker build --rm --no-cache -t jps-chatbot:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To run the image and generate a container:
	docker run -d -p 5000:5000 --expose 5000 --restart always --name "jps-chatbot" -it jps-chatbot:`git rev-parse --short HEAD`
	