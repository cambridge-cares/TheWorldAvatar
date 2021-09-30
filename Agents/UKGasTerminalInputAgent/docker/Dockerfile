#########################
#
# This Dockerfile creates an image with an environment appropriate for running the Gas Grid Agent.
# It builds on the Python agent example located at '/Deploy/examples/python_agent'.
#
# See the README for more details.
#
#########################

FROM python:3.7.10 as common

# Meta data
LABEL authors = "support@cmclinnovations.com, trs53@cam.ac.uk, mh807<@>cam.ac.uk"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Gas Grid Agent"

# Install utilities
RUN apt update && apt-get install -y rsyslog cron procps nano dos2unix wget software-properties-common

# Install Java 8
RUN wget -qO - https://adoptopenjdk.jfrog.io/adoptopenjdk/api/gpg/key/public | apt-key add -
RUN add-apt-repository --yes https://adoptopenjdk.jfrog.io/adoptopenjdk/deb/
RUN apt update && apt install -y adoptopenjdk-8-hotspot

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Copy in everything
COPY . /root

# Install web server (to host geojson files for download)
RUN apt update && apt install -y apache2
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN mkdir -p /var/www/html/gas-grid
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Permissions & EOL
WORKDIR /root
RUN chmod -R 755 ./
RUN dos2unix docker/start-up.sh
RUN dos2unix docker/cron-jobs

# Make a directory for the gas-grid logs
RUN mkdir /var/log/gas-grid
RUN chmod 777 /var/log/gas-grid

# Set environment variables
ENV PYTHONPATH=/usr/local/lib/python3.7/site-packages

# Expose port for HTTP server
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "sh", "-c", "rsyslogd && /app/start-up.sh" ]

