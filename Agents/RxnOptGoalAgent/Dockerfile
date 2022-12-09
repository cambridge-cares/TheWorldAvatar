# Install the RxnOptGoalAgent in docker container
# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
# Base image is a lightweight version of Python
FROM python:3.8-slim-buster as base

# Meta data
# NOTE change the label to your email and description
LABEL authors = "jb2197@cam.ac.uk"
LABEL description = "rxnoptgoalagent is capable of instantiating and managing iterations of pursuring the reaction optimisation goal as part of The World Avatar project."

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
WORKDIR /app
ADD ./$AGENTDIR ./$AGENTDIR
# Also copy the templates folder for flask
ADD ./templates ./templates

# Install the agent package itself as editable version
# This seems to be required for the sparql_client in the URL routes to be in the same gateway as __init__
RUN python -m pip install -e .

# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
RUN useradd appuser && chown -R appuser /app

# Creating the user doesn't create their home directory for some reason, so create it now
RUN mkdir /home/appuser
RUN chown -R appuser:appuser /home/appuser
RUN chmod -R 755 /home/appuser

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'python_agent' module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
USER appuser
CMD gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()"


#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base as dev
# Install additional dependencies
RUN python -m pip install -e .[dev]
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/${AGENTDIR}:create_app()
ENV FLASK_DEBUG=1

# Start dockerised agent app
WORKDIR /app
CMD python -m debugpy --listen 0.0.0.0:5678 -m flask run -h 0.0.0.0 -p 5000


# #------------------------------------------------------
# # Debug testing image reusing the base
# #------------------------------------------------------
# FROM base as test_dockerised_debug
# # Install additional dependencies
# RUN python -m pip install -e .[dev]
# RUN pip install debugpy

# # Set flask app and development environment to allow for hot-reloading
# ENV FLASK_APP=app/${AGENTDIR}:create_app()
# ENV FLASK_DEBUG=1
# # Set the environment variable DOCKERISED_TEST as true to enable dockerised testing
# ENV DOCKERISED_TEST=true

# # Copy docker-compose-file to spin up testcontainers (agent code and tests are mounted as volumes)
# COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# # Launch pytest in container (-s flag to enable more output/logging)
# WORKDIR /app
# # Start dockerised agent app as well as pytest
# # (combining commands using & ensures they are started in parallel in background)
# CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000 & pytest -s


# #------------------------------------------------------
# # Testing image reusing the base
# #------------------------------------------------------
# FROM base as test_dockerised

# # Install agent package as editable
# RUN python -m pip install -e .[dev]

# # Set the default working directory, then copy the Python source code into it
# # As here folder path is generic, it is not required to change the name
# WORKDIR /app
# COPY ./$AGENTDIR ./$AGENTDIR
# COPY ./tests ./tests
# # Copy docker-compose-file to spin up testcontainers
# COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# # Set the environment variable DOCKERISED_TEST as true to enable dockerised testing
# ENV DOCKERISED_TEST=true

# # Start dockerised agent app as well as pytest
# # (combining commands using & ensures they are started in parallel in background)
# CMD gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()" & pytest

#==================================================================================================
