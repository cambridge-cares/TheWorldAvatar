# KineticsAgent

This directory contains the files required to build a Docker Image for execution of the KineticsAgent. Note that this setup is currently configured to use the KineticsAgent for the SimDOME project, as more capability is added to the agent in future, this setup will need updating.

## Requirements

The following steps must be completed before attempting to build the KineticsAgent Image.

1. Make a copy of the *settings-template.xml* and name it *settings.xml*.
2. Add your Nexus username and password to the *settings.xml* file.
	* Contact CMCL administrators for a username and password if you don't already have one.
3. Build the KineticsAgent.
	* Locally build the KineticsAgent project into the KineticsAgent.war file.
	* Before building, set the below values within the *kinetics-agent.properties* resource file. Do not commit these changes to the repository.

* hpc.server.loging.user.name=[ASK CMCL]
* hpc.server.loging.user.password=[ASK CMCL]
* hpc.address=[ASK CMCL]
* slurm.script.file=docker-slurm.sh
* agent.scripts.location=/usr/local/simdome

4. Update the *agent* stack of docker-compose files to ensure the details are still correct.
	* If the version number of the KineticsAgent has been updated, this will need to be updated within the stack too.

## Building the Image

Once the requirements have been addressed, the Image can be build using the following methods.

+ Use the *start-stack.sh* scripts with the *agent dev* arguments to build the entire stack.
 + Using the *prod* mode will only download pre-built Images, the *dev* mode must be used to build Images from scratch.
+ Use the below command from this directory to build the KineticsAgent Image in isolation.
 + `docker build --rm --no-cache -t kinetics-agent .`
 
## Improvements

To better facilitate the setup of the KineticsAgent Image, the following improvements to the source code should be considered.

1. Change the *kinetics-agent.properties* file to become an external resource.
	* Currently, as it's an internal resource that defines each HPC setup, a new release of the WAR file has to take place for each deployment setup. If the properties file is moved to an external resource (that sits along side the WAR file), then the compiled source code can be used for all HPCs.
2. Upload the built KineticsAgent.war file to the Nexus server at CMCL.
	* This keeps a record of each version of the WAR file, and would also allow other developers to download the file without having to build the source code (and all it's prerequisites).