# Using Alpine as the base image
FROM  alpine:3.14.0 as dtvf

# Install utilities
RUN apk update && apk add procps nano wget bash busybox-initscripts dos2unix

# Install Apache and PHP
RUN apk update && apk add apache2 php7-apache2

# Copy in the start-up script
COPY ./docker/start-up.sh /usr/local/start-up.sh
RUN chmod 777 /usr/local/start-up.sh
RUN chmod +x /usr/local/start-up.sh
RUN dos2unix /usr/local/start-up.sh

# Ensure webspace directory exists
RUN mkdir -p /var/www/html

# Custom HTTPD configuration
COPY docker/httpd.conf /etc/apache2/httpd.conf

# Permissions
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "/bin/bash", "-c", "/usr/local/start-up.sh" ]