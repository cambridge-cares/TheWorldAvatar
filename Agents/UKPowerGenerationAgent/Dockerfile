# Installs the UK Power Generation Agent in a Docker container
#==================================================================================================

# Creates a new build stage called `stackclients' based on the published
# Docker image `docker.cmclinnovations.com/stack-client' with the tag `1.6.2'
# to use resources from `stack-client'
FROM docker.cmclinnovations.com/stack-client:1.6.2 as stackclients

# Creates a new build stage called `base' depending on the published
# Docker image `python' in Docker Hub registry with the tag
# `3.7.11-slim-buster' to use a slim version of Python'
FROM python:3.7.11-slim-buster as base
# Adds the authors and description meta data to the image being built
LABEL authors = "msff2@cam.ac.uk"
LABEL description = "UK Power Generation Agent"

# Sets an environment variable in the Docker image to keep Python from
# generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Sets an environment variable in the Docker image to turn off
# buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Installs OpenJDK 11 and Git
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Creates a folder called `tmp_stack'
RUN mkdir ./tmp_stack
# Copies the `stack-client' resources from the `app' directory to `tmp_stack'
COPY --from=stackclients /app ./tmp_stack

# Sets the default working directory to `app' & copies files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./setup.py .
COPY ./resources/ontop.obda ./resources/ontop.obda
ADD https://jdbc.postgresql.org/download/postgresql-42.5.0.jar ./resources/

# Upgrades the pip Python package manager to its latest version
RUN python -m pip install --upgrade pip
# Installs a Python package in editable mode
RUN pip install -e .

# Installs the StackClients jar file taking it from the directory `tmp_stack'
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Exposes a port
EXPOSE 5000

#------------------------------------------------------
# Debugging image reusing the base
#------------------------------------------------------
FROM base as debug
# Installs additional dependencies for debugging
RUN pip install debugpy

# Sets flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/agent/flaskapp/wsgi.py
ENV FLASK_DEBUG=1

# Sets the working directory to `app/agent/flaskapp'
WORKDIR /app/agent/flaskapp/
# Executes this command to run the `debugpy' server module and `flask'
# application module in the container
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod

# Sets the working directory to `app'
WORKDIR /app
# Copies required source code
COPY ./agent ./agent
COPY ./app_entry_point.sh .

# Sets the entrypoint to `app_entry_point.sh'
ENTRYPOINT ./app_entry_point.sh
#==================================================================================================