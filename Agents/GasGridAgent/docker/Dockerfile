#########################
#
# This Dockerfile creates an image with an environment appropriate for running the Gas Grid Agent.
# It builds on the Python agent example located at '/Deploy/examples/python_agent'.
#
# See the README for more details.
#
#########################

# Use Python as a base image
FROM python:3.7.10 as common

# Meta data
LABEL authors = "support<@>cmclinnovations.com, trs53<@>cam.ac.uk, mh807<@>cam.ac.uk"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Gas Grid Agent"

# Install utilities
RUN apt update && apt-get install -y rsyslog cron bash procps nano dos2unix wget software-properties-common

# Install Java 8 (for JPS base library)
RUN wget -qO - https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | apt-key add -
RUN add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
RUN apt update && apt install -y adoptopenjdk-8-hotspot

# Install web server (to host geojson files for download)
RUN apt update && apt install -y apache2
COPY ./docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN mkdir -p /var/www/html/gas-grid
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Copy in required scripts/files
COPY ./docker/start-up.sh /root/start-up.sh
COPY ./docker/run-outputs.sh /root/run-outputs.sh
COPY ./docker/cron-jobs /root/cron-jobs

# Copy in the Python source code
COPY ./src /root/code
RUN chmod +x /root/code/install_script_pip.sh
RUN dos2unix /root/code/install_script_pip.sh

# Permissions & line endings
WORKDIR /root
RUN chmod -R 755 ./
RUN dos2unix /root/start-up.sh
RUN dos2unix /root/run-outputs.sh
RUN dos2unix /root/cron-jobs

# Make a directory for the gas-grid logs
RUN mkdir /var/log/gas-grid
RUN chmod 755 /var/log/gas-grid

# Set environment variables
ENV PYTHONPATH=/usr/local/lib/python3.7/site-packages

# Expose port for HTTP server
EXPOSE 80

# Run cron daemon and boot script at start
ENTRYPOINT [ "/bin/bash", "/root/start-up.sh" ]

