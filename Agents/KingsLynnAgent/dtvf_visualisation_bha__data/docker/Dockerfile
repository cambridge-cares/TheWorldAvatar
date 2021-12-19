#########################
#
# This docker file builds an image for the Example Digital Twin visualistion.
# It can also be used as starting point for any custom DT visualisations that
# are created in future.
#
# NOTE: The "docker build" command used to build this file into an Image should
# be run from the "example-dt-vis" folder, not from within this "docker" directory.
# See the README for more details.
# 
# Stages:
#	- dev:	vis will use development versions of remote JS and CSS
#	- prod:	vis will use production versions of remote JS and CSS
#
#########################


##### DEVELOPMENT STAGE #####
# Using Alpine as the base image
FROM  alpine:3.14.0 as dev

# Meta data
LABEL authors = "support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Example Digital Twin"

# Install utilities
RUN apk update && apk add procps nano wget bash busybox-initscripts dos2unix

# Install Apache and PHP
RUN apk update && apk add apache2 php7-apache2

# Copy in the start-up script
COPY ./docker/start-up.sh /usr/local/start-up.sh
RUN chmod 777 /usr/local/start-up.sh
RUN chmod +x /usr/local/start-up.sh
RUN dos2unix /usr/local/start-up.sh

# Copy in the files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html
COPY . .

# Custom HTTPD configuration
COPY docker/httpd.conf /etc/apache2/httpd.conf

# Permissions
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Copy in the HTML file that designates the DEVELOPMENT imports
#COPY head-dev.html head.html

# Run cron daemon and boot script at start
CMD [ "/bin/bash", "-c", "/usr/local/start-up.sh" ]


##### PRODUCTION STAGE #####
# Using development image as the base 
FROM dev as prod

# Copy in the HTML file that designates the PRODUCTION imports
#COPY head-prod.html head.html
