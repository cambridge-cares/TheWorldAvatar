# Gas Grid Agent

The folder contains source, resource, and Docker setup files for the Gas Grid Agent. This agent forms part of the `agent` stack at CMCL.

The "Dockerfile" file contains the instructions to build an image; before making any changes to it, please consult the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

## Purpose

The Gas Grid agent exists to download, parse, and add live gas flow data to the Knowledge Graph. It does this via the `input_flow_data.py` script; once started, every 12 minutes this script will download a flow data CSV, parse it, then upload it to the KG endpoint specified within the properties file. This is done using the new TimeSeries format so the TimeSeriesClient class from the JPS Base Library is also used to check for and, where needed, instantiate new TimeSeries links within the KG.

This agent also contains scripts to query the KG and output the location of Terminals, Offtakes, and Pipes to local GeoJSON files. These scripts are run once per day (via cron), and output to the `/var/www/html/gas-grid` directory. The Gas Grid visualisation (which runs within a separate Docker container), can then download these files (via wget) ensuring that if new locations are added to the KG, no manual updates need to take place to add them to the ecosystem.

The Docker configuration of this Agent also sets the `EMAIL_AGENT_URL` environment variable so that the `EmailSender` class of the JPS Base Library can be used to send notification emails whenever a critical error occurs.

## Building the Image

Requirements:
+ The Python code of this Agent requires the `py4jps` library, this is currently downloaded from the external PyPi website. This means that if a specific version of the Python Wrapper, or JPS Base Library is needed, the `py4jps` library needs to be
 rebuilt and reuploaded to PyPi first.
+ The KG endpoints, and TimeSeries database used by this agent are specified within the `resources/gasgridagent.properties` file. Developers needs to ensure that this file is correctly updated before building the Docker Image.
Once the requirements have been addressed, the Image can be build using the following methods. Note that once this code has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

To build the image as part of the `agent` stack, the `/Deploy/stacks/build-stack.sh` should be used (please refer to the script documentation). For convenience, commands to build the Gas Grid Agent image in isolation are listed below (note that these should be run from the `/Deploy/stacks` directory and target the `dev` environment).

+ To build the image:
  + `build-stack.sh agent dev gasgridagent`
+ To generate a container:
  + `start-stack.sh agent dev gasgridagent`