# Install the Forecasting Agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use
FROM ghcr.io/cambridge-cares/stack-client:1.20.1 as stackclients

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster as base
# Meta data
LABEL authors = "mh807@cam.ac.uk, mm2692@cam.ac.uk"
LABEL description = "Forecasting Agent"

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
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn==20.0.4

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./forecastingagent ./forecastingagent

# Set the entrypoint
# timeout set to 30min to avoid exceptions for longer API/KG calls
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "forecastingagent:create_app()" --timeout 1800

#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base as test
# Install additional dependencies
RUN apt-get update
RUN apt install libpq-dev python3-dev gcc -y
RUN python -m pip install -e .[dev]
RUN pip install gunicorn==20.0.4

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./forecastingagent ./forecastingagent
COPY ./tests ./tests

# Copy docker-compose-file to spin up testcontainers
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Adjust the logger_name to enable more logging information for tests
RUN sed -i "s|logger_name='prod',|logger_name='dev',|g" ./forecastingagent/entry_point.py

# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
CMD gunicorn --bind 0.0.0.0:5000 "forecastingagent:create_app()" & pytest

#------------------------------------------------------
# Debug testing image reusing the base
#------------------------------------------------------
FROM base as debug_test
# Install additional dependencies
RUN apt-get update
RUN apt install libpq-dev python3-dev gcc -y
RUN python -m pip install -e .[dev]
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/forecastingagent:create_app()
ENV FLASK_DEBUG=1

# Copy docker-compose-file to spin up testcontainers (agent code and tests are mounted as volumes)
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Launch pytest in container (-s flag to enable more output/logging)
WORKDIR /app

# CMD will be updated in the docker-compose-test_dockerised_debug.yml file in order to create
# a debugpy session for the agent app and run pytest in parallel
CMD ["/bin/bash"]

#==================================================================================================