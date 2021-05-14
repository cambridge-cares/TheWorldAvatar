---------- DESCRIPTION ----------

The folder contains the required files to build a Docker image for the JPS Chatbot. The "Dockerfile"
file contains the instructions to build a JPS Chatbot image; before making any changes to it, please
consult the application's developer (Xiaochi Zhou <xz378@cam.ac.uk>) and the system administrators
at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before continuing:

	- The chatbot installed within the Docker image will be based on the current commit 
	of this repository, please ensure you're on the right one.

	- Read the README within the credentials directory for details on how to setup access
	to Vienna so that model files can be downloaded. This MUST be completed BEFORE
	attempting to build this Image.
	
	- Building this Image should really be done using the web stack configuration within the
	"Deploy/stacks/web" directory; commands below can be used during local development.
	
	- If spinning up in isolation, you may need to change the Docker network that the Container
	joins (using the "--network" arg) so that other Containers can call it.
	
	- If spinning up in isolation, the "docker build" command should be run from the JPS_Chatbot
	directory (not this one); this is so that a copy of the JPS_Chatbot directory can be copied
	into the image.
	
	
---------- COMMANDS ----------
	
To build the image:
	docker build --rm --no-cache -t jps-chatbot:`git rev-parse --short HEAD` -f docker/Dockerfile .
	
To run the image and generate a container:
	docker run -d -p 5000:5000 --expose 5000 --restart always --name "jps-chatbot" -it jps-chatbot:`git rev-parse --short HEAD`
	
	
---------- TODO ----------

Once the chatbot developer has assigned a version to the tool, Images should be built with that version (appending -SNAPSHOT
for development versions) rather than using the Git commit hash shown in the commands above.