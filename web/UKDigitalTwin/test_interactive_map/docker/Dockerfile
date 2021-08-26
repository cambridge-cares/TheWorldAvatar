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

# Base image is a lightweight version of Python
FROM python:3.7.11-slim-buster

# Meta data
LABEL authors = "wx243@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "1.0.0-SNAPSHOT"
LABEL description = "Power System Visualisation"

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless

# Install utilities
RUN apt update && apt-get install -y cron procps nano dos2unix

# Install web server (to host geojson files for download)
RUN apt update && apt install -y apache2
RUN mkdir -p /var/www/html/
RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Copy in the files
WORKDIR /var/www/html
COPY . .

# Install the required Python libraries
COPY docker/requirements.txt .
RUN python -m pip install -r requirements.txt

# Set the default working directory, then copy the Python source code into it
COPY docker/start-up.sh /usr/local/start-up.sh
RUN chmod 755 /usr/local/start-up.sh
RUN dos2unix /usr/local/start-up.sh

RUN chown -R www-data:www-data /var/www/
RUN chmod -R 775 /var/www/

# Expose port 80
EXPOSE 80

# Run cron daemon and boot script at start
CMD [ "/bin/bash", "-c", "/usr/local/start-up.sh && tail -f /dev/null" ]

# Finish
RUN echo -e "Image installation completed."
