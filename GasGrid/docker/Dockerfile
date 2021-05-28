#########################
#
# This docker file creates an Image with an environment
# for running the Gas Grid Agent.
#
# This Dockerfile builds on the Python agent example
# (located at '/Deploy/examples/python_agent').
#
# See the README for more details.
#
#########################

# Base image is a lightweight version of Python
FROM python:3.7.10

# Install Java
RUN apt update && apt install -y openjdk-11-jdk

# Install cron
RUN apt update && apt-get install -y cron procps nano

# Install web server (to host geojson files for download)
RUN apt update && apt install -y apache2
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
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
COPY docker/start-up.sh start-up.sh
RUN chmod 755 start-up.sh

# Copy in the cron jobs file
COPY docker/cron-jobs cron-jobs 
RUN chmod 755 cron-jobs 

# Scripts relating to gathering data and inputting it to the KG
RUN mkdir input
COPY OntoGasGrid/real_time_terminal_population/terminal-update.py input/terminal-update.py
RUN chmod +x input/terminal-update.py

# Scripts relating to querying data in the KG and formatting it
RUN mkdir output
COPY OntoGasGrid/geoJSON_output_agent/all_offtakes_to_geojson.py output/all_offtakes_to_geojson.py
COPY OntoGasGrid/geoJSON_output_agent/all_pipes_query_to_geojson.py output/all_pipes_query_to_geojson.py
COPY OntoGasGrid/geoJSON_output_agent/all_terminals_to_geojson.py output/all_terminals_to_geojson.py
RUN chmod -R 755 ./output

# Make a directory for the gas-grid logs
RUN mkdir /var/log/gas-grid

# Set environment variable
# Note that this should really be done from the compose file
ENV TARGET_MODE="CMCL"

# Expose port for HTTP server
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "sh", "-c", "cron && /app/start-up.sh" ]
#CMD [ "sh", "-c", "cron && tail -f /dev/null" ]