#########################
#
# This docker file creates an image for the combined
# Digital Twin visualisation.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the root project folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# The following stages are included:
# 
#	production			Required software only
#
#########################


##### PRODUCTION STAGE #####
# Using Apache HTTP as the base 
FROM httpd:2.4.46-alpine as production

# Meta data
LABEL authors = "support@cmclinnovations.com"
LABEL description = "Digital Twin"

# Create directory for files
RUN mkdir -p /var/www/html
WORKDIR /var/www/html

# Copy in the files
COPY ./src .
COPY docker/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Finish
RUN echo -e "Image installation completed."