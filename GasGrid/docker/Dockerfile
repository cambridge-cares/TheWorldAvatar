#########################
#
# This Dockerfile creates an image with an environment appropriate for running the Gas Grid Agent.
# It builds on the Python agent example located at '/Deploy/examples/python_agent'.
#
# See the README for more details.
#
#########################


#====================== Common configuration shared by 'dev' and 'prod' images ====================
FROM python:3.7.10 as common

# Meta data
LABEL authors = "trs53@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Gas Grid Agent"

# Install utilities
RUN apt update && apt-get install -y rsyslog cron procps nano dos2unix wget software-properties-common

# Install Java 8
RUN wget -qO - https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | apt-key add -
RUN add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
RUN apt update && apt install -y adoptopenjdk-8-hotspot

# Install web server (to host geojson files for download)
RUN apt update && apt install -y apache2
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN mkdir -p /var/www/html/gas-grid
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY docker/requirements.txt .
RUN python -m pip install -r requirements.txt

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY docker/start-up.sh /app/start-up.sh
RUN chmod 755 /app/start-up.sh
RUN dos2unix /app/start-up.sh

# Copy in the cron jobs file
COPY docker/cron-jobs cron-jobs 
RUN chmod 755 cron-jobs 
RUN dos2unix cron-jobs

# Scripts relating to gathering data and inputting it to the KG
RUN mkdir input
COPY OntoGasGrid/real_time_terminal_population/terminal-update.py input/terminal-update.py
RUN chmod +x input/terminal-update.py

# Scripts relating to querying data in the KG and formatting it
RUN mkdir output
COPY docker/run-outputs.sh /app/output/run-outputs.sh
RUN dos2unix /app/output/run-outputs.sh
COPY OntoGasGrid/flow_data_generator/output_flow_data.py output/output_flow_data.py
COPY OntoGasGrid/geoJSON_output_agent/all_offtakes_to_geojson.py output/all_offtakes_to_geojson.py
COPY OntoGasGrid/geoJSON_output_agent/all_pipes_query_to_geojson.py output/all_pipes_query_to_geojson.py
COPY OntoGasGrid/geoJSON_output_agent/all_terminals_to_geojson.py output/all_terminals_to_geojson.py
RUN chmod -R 755 ./output

# Make a directory for the gas-grid logs
RUN mkdir /var/log/gas-grid
RUN chmod 777 /var/log/gas-grid

# Set environment variables
# TODO: Read this from the docker-compose file in future.
ENV TARGET_MODE="CMCL"
ENV PYTHONPATH=/usr/local/lib/python3.7/site-packages

# Expose port for HTTP server
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "sh", "-c", "rsyslogd && /app/start-up.sh" ]


#===================================== Development configuration ==================================
FROM common as dev
ENV KG_LOCATION="http://kg.cmclinnovations.com:81/blazegraph_geo"


#===================================== Production configuration ===================================
FROM common as prod
ENV KG_LOCATION="https://kg.cmclinnovations.com/blazegraph_geo"
