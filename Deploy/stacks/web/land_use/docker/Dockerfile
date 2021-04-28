#########################
#
# This docker file creates an image for the "AI Implementation
# of Biomass Availability and Land Use.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the sustainable_goals folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# The following stages are included:
# 
#	base			Required software only
#
#########################

##### PRODUCTION STAGE #####
# Using Apache HTTP as the base 
FROM httpd:2.4.46-alpine as base

# Meta data
LABEL authors = "zh305@cam.ac.uk, dnls2@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "0.0.1"
LABEL description = "Biomass Availability & Land Use"

# Create directory for files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html

# Copy in the files
COPY . .
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Finish
RUN echo -e "Image installation completed."