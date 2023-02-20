# Install the Derivation Agent Python Example in Docker container
# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python
#==================================================================================================

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
# Base image is a lightweight version of Python
FROM python:3.8-slim-buster as base

# Meta data
# NOTE change the label to your email and description
LABEL authors = "jx309@cam.ac.uk"
LABEL description = "A simple toy agent to calculate the age based on birthday."

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install required software: git and openjdk-11
USER root
RUN apt update && apt install -y git openjdk-11-jdk-headless

# Expose the port on which our server will run
EXPOSE 5000

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install -r requirements.txt

# # Re-install the version of JPS_BASE_LIB that is been developing
# # (this will be required if the new features are not merged back to main)
# RUN jpsrm uninstall JpsBaseLib
# RUN mkdir /jpstemp
# COPY /_temp/jps-base-lib.jar ./jpstemp/jps-base-lib.jar
# COPY /_temp/lib ./jpstemp/lib
# RUN jpsrm install JpsBaseLib ./jpstemp/

# Set the default working directory, then copy files for python package install
WORKDIR /app
COPY ./README.md .
COPY ./setup.py .

# Access build arguments from docker-compose.yml
# (Use ENV instead of ARG for variable to be available in ENTRYPOINT command)
ARG agentdir
ENV AGENTDIR=${agentdir}

# Install Python package and the required libraries
RUN python -m pip install --upgrade pip
RUN python -m pip install -e .


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod

# Set the default working directory, then copy the Python source code into it
# As here folder path is generic, it is not required to change the name
WORKDIR /app
COPY ./$AGENTDIR ./$AGENTDIR

# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
RUN useradd appuser && chown -R appuser /app

# Creating the user doesn't create their home directory for some reason, so create it now
RUN mkdir /home/appuser
RUN chown -R appuser:appuser /home/appuser
RUN chmod -R 755 /home/appuser

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the agent module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
USER appuser
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()"


#------------------------------------------------------
# Debug testing image reusing the base
#------------------------------------------------------
FROM base as test_dockerised_debug
# Install additional dependencies
RUN python -m pip install -e .[dev]
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/${AGENTDIR}:create_app()
ENV FLASK_DEBUG=1
# Set the environment variable DOCKERISED_TEST as true to enable dockerised testing
ENV DOCKERISED_TEST=true

# Copy docker-compose-file to spin up testcontainers (agent code and tests are mounted as volumes)
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Launch pytest in container (-s flag to enable more output/logging)
WORKDIR /app
# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000 & pytest -s


#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base as test_dockerised

# Install agent package as editable
RUN python -m pip install -e .[dev]

# Set the default working directory, then copy the Python source code into it
# As here folder path is generic, it is not required to change the name
WORKDIR /app
COPY ./$AGENTDIR ./$AGENTDIR
COPY ./tests ./tests
# Copy docker-compose-file to spin up testcontainers
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Set the environment variable DOCKERISED_TEST as true to enable dockerised testing
ENV DOCKERISED_TEST=true

# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
CMD gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()" & pytest

#==================================================================================================
