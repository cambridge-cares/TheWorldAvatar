# Install the Emission Agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use
FROM ghcr.io/cambridge-cares/stack-client:1.20.1 AS stackclients

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster AS base
# Meta data
LABEL authors = "mh807@cam.ac.uk"
LABEL description = "Emission Agent"

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1
# Disable pip cache to keep image size as small as possible
ENV PIP_NO_CACHE_DIR=false

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy Stack-Client resource from published Docker image
COPY --from=stackclients /app ./tmp_stack

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./requirements.txt .
COPY ./setup.py .

# Install Python package and the required libraries 
# (use pinned versions from requirements.txt to avoid potential clashes before installing package itself)
RUN python -m pip install --upgrade pip
RUN python -m pip install -r requirements.txt
RUN python -m pip install -e .

# Update py4jps resources
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Expose port
EXPOSE 5000

#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base AS prod
# Install additional dependencies
RUN pip install gunicorn==20.0.4

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./emissionagent ./emissionagent

# Set the entrypoint: timeout set to 5min to avoid exceptions for 
# longer API/KG calls, but still be considered "synchronous"
# NOTE: Remove gunicorn server request length limit to avoid that HTTP GET requests
#       to update synchronous derivations are rejected by the server (to be used with caution!)
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "emissionagent:create_app()" --timeout 300 --limit-request-line 0

#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base AS test
# Install additional dependencies
RUN apt-get update
RUN apt install libpq-dev python3-dev gcc -y
RUN python -m pip install -e .[dev]
RUN pip install gunicorn==20.0.4

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./emissionagent ./emissionagent
COPY ./tests ./tests

# Copy docker-compose-file to spin up testcontainers
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Adjust the logger_name to enable more logging information for tests
RUN sed -i "s|logger_name='prod',|logger_name='dev',|g" ./emissionagent/entry_point.py

# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
# NOTE: Remove gunicorn server request length limit to avoid that HTTP GET requests
#       to update synchronous derivations are rejected by the server (to be used with caution!)
CMD gunicorn --bind 0.0.0.0:5000 "emissionagent:create_app()" --limit-request-line 0 & pytest

#------------------------------------------------------
# Debug testing image reusing the base
#------------------------------------------------------
FROM base AS debug_test
# Install additional dependencies
RUN apt-get update
RUN apt install libpq-dev python3-dev gcc -y
RUN python -m pip install -e .[dev]
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/emissionagent:create_app()
ENV FLASK_DEBUG=1

# Expose the debugging ports to debug agent (5678) and tests (5679)
# NOTE: 5678 should be open by default anyway, just for reference
EXPOSE 5678
EXPOSE 5679

# Copy docker-compose-file to spin up testcontainers (agent code and tests are mounted as volumes)
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Launch pytest in container (-s flag to enable more output/logging)
WORKDIR /app

# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
CMD python -m debugpy --listen 0.0.0.0:5678 -m flask run -h 0.0.0.0 -p 5000 & \
    python -m debugpy --listen 0.0.0.0:5679 --wait-for-client -m pytest -s

#==================================================================================================