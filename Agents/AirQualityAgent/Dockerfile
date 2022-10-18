# Install the Air Quality agent in Docker container
#==================================================================================================

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9-slim-buster as base
# Meta data
LABEL authors = "mh807@cam.ac.uk"
LABEL description = "UK-AIR Sensor Observation Service agent"

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./setup.py .
COPY ./resources/airquality.properties ./resources/airquality.properties

# Install Python package and the required libraries
RUN pip install -e .
# Installation of agentlogging currently suspended due to performance issues
#RUN pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"

# Expose port
EXPOSE 5000

#------------------------------------------------------
# Debugging image reusing the base
#------------------------------------------------------
FROM base as debug
# Install additional dependencies for debugging
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/airquality/flaskapp/wsgi.py
ENV FLASK_ENV=development

# Launch application in container
WORKDIR /app/airquality/flaskapp/
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./airquality ./airquality
COPY ./app_entry_point.sh .

# Set the entrypoint
ENTRYPOINT ./app_entry_point.sh
#==================================================================================================