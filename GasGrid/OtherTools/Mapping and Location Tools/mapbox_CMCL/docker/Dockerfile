#########################
#
# This docker file creates an image for the UK Gas Grid visualisation.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the mapbox_CMCL folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# The following stages are included:
# 
#	production			Required software only
#
#########################

##### CREDENTIALS STAGE ######
FROM maven:3.6.3-openjdk-11 as credentials

# Copy across Maven files
RUN mkdir -p /tmp/geoJSON_assets
WORKDIR /tmp
COPY docker/pom.xml pom.xml
COPY docker/settings.xml settings.xml

# Run Maven project to download resources
RUN mvn -X install -s settings.xml

##### PRODUCTION STAGE #####
# Using Apache HTTP as the base 
FROM httpd:2.4.46-alpine as production

# Meta data
LABEL authors = "trs53@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "0.0.1"
LABEL description = "UK Gas Grid"

# Create directory for files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html

# Copy in the model files downloaded in last stage
COPY --from=credentials /tmp/geoJSON_assets /var/www/html/geoJSON_assets

# Copy in the files
COPY . .
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Finish
RUN echo -e "Image installation completed."