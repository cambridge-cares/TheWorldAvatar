#########################
#
# This docker file creates an image for the Land Use visualisation.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the land_use folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# Stages:
#	- development:	uses development JS/CSS imports
#	- production:	uses production JS/CSS imports
#
#########################

##### DEVELOPMENT STAGE #####
# Using Alpine as the base 
FROM  alpine:3.14.0 as development

# Meta data
LABEL authors = "zh305@cam.ac.uk, dnls2@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Land Use Visualisation"

# Install utilities
RUN apk update && apk add procps nano bash dos2unix

# Install Apache and PHP
RUn apk update && apk add apache2 php7-apache2

# Copy in the files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html
COPY . .

# Copy in the head.html specific to the DEVELOPMENT setup
COPY head-dev.html head.html

# Custom HTTPD configuration
COPY docker/httpd.conf /etc/apache2/httpd.conf

# Permissions
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Run apache at start
CMD [ "httpd" , "-D", "FOREGROUND"]

##### PRODUCTION STAGE #####
# Using development as the base 
FROM development as production

# Copy in the head.html specific to the PRODUCTION setup
COPY head-prod.html head.html