# From the GoAccess base image
FROM allinurl/goaccess:1.4.6

# Install Apache webserver
#RUN apt-get update && apt-get install -y apache2
#EXPOSE 80

# Handle web documents folder
#RUN mkdir -p /var/www/html
#RUN chown -R www-data:www-data /var/www/
#RUN chmod -R 775 /var/www/

# Entrypoint to keep the container running
ENTRYPOINT ["tail", "-f", "/dev/null"]
