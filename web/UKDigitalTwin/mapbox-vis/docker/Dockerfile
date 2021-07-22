#########################
#
# This docker file creates an image for the Power System visualisation.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the mapbox-vis folder,
# not from within the "docker" directory. See the README
# for more details.
#
#########################

##### DEVELOPMENT STAGE #####
# Base image is a lightweight version of Python
FROM python:3.7.10 as development

# Meta data
LABEL authors = "trs53@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Power System Visualisation"

# Install utilities
RUN apt update && apt-get install -y cron procps nano dos2unix

# Install PHP
RUN apt update && apt-get install -y php libapache2-mod-php

# Install web server 
RUN apt update && apt install -y apache2
RUN mkdir -p /var/www/html/
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Copy in custom Apache configuration
COPY apache2.conf /etc/apache2/apache2.conf
RUN dos2unix /etc/apache2/apache2.conf
RUN chmod 644 /etc/apache2/apache2.conf

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY docker/requirements.txt .
RUN python -m pip install -r requirements.txt

# Set the default working directory, then copy the start-up script
COPY docker/start-up.sh /usr/local/start-up.sh
RUN chmod 755 /usr/local/start-up.sh
RUN dos2unix /usr/local/start-up.sh

# Copy in the files
WORKDIR /var/www/html
COPY . .

# Copy in the head.html specific to the DEVELOPMENT setup
COPY head-dev.html head.html

RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "/bin/bash", "-c", "/usr/local/start-up.sh && tail -f /dev/null" ]


##### PRODUCTION STAGE #####
# Using development as the base 
FROM development as production

# Copy in the download script specific to the PRODUCTION setup
COPY head-prod.html head.html
RUN chmod 775 head.html
