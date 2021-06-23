#########################
#
# This docker file creates an image for the UK Gas Grid visualisation.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the mapbox_CMCL folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# Stages:
#	- development:	copies in download-dev.sh
#	- production:	copies in download-prod.sh
#
#########################

##### DEVELOPMENT STAGE #####
# Using Alpine as the base 
FROM  alpine:3.14.0 as development

# Meta data
LABEL authors = "trs53@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Gas Grid Visualisation"

# Install utilities
RUN apk update && apk add procps nano wget bash busybox-initscripts dos2unix

# Install Apache and PHP
RUn apk update && apk add apache2 php7-apache2

# Make a directory for the gas-grid logs
RUN mkdir /var/log/gas-grid

# Copy in the download script specific to the DEVELOPMENT setup
COPY docker/download-dev.sh /usr/local/download.sh
RUN chmod 755 /usr/local/download.sh
RUN dos2unix /usr/local/download.sh

# Copy in the start-up script
COPY docker/start-up.sh /usr/local/start-up.sh
RUN chmod 777 /usr/local/start-up.sh
RUN chmod +x /usr/local/start-up.sh
RUN dos2unix /usr/local/start-up.sh

# Copy in the cron-jobs file
COPY docker/cron-jobs /usr/local/cron-jobs
RUN chmod 755 /usr/local/cron-jobs

# Copy in the files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html
COPY . .

# Copy in the head.html specific to the DEVELOPMENT setup
COPY head-dev.html head.html

# Custom HTTPD configuration
COPY docker/httpd.conf /etc/apache2/httpd.conf

# Permissions
#RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "/bin/bash", "-c", "/usr/local/start-up.sh && exec crond -f" ]


##### PRODUCTION STAGE #####
# Using development as the base 
FROM development as production

# Copy in the download script specific to the PRODUCTION setup
COPY docker/download-prod.sh /usr/local/download.sh
RUN chmod 755 /usr/local/download.sh
RUN dos2unix /usr/local/download.sh

# Copy in the head.html specific to the PRODUCTION setup
COPY head-prod.html head.html
