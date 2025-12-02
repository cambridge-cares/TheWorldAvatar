# Install PubChem agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use
FROM ghcr.io/theworldavatar/stack-client${IMAGE_SUFFIX}:1.53.0 as stackclients

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9-slim-buster as base
RUN set -eux; \
    sed -i 's|deb.debian.org/debian|archive.debian.org/debian|g' /etc/apt/sources.list; \
    sed -i 's|security.debian.org/debian-security|archive.debian.org/debian-security|g' /etc/apt/sources.list; \
    printf 'Acquire::Check-Valid-Until "false";\nAcquire::AllowInsecureRepositories "true";\n' \
      > /etc/apt/apt.conf.d/99archive; \
    apt-get -o Acquire::Check-Valid-Until=false update; \
    apt-get install -y --no-install-recommends openjdk-11-jdk-headless; \
    rm -rf /var/lib/apt/lists/*
# Meta data
LABEL authors = "lp521@cam.ac.uk"
LABEL description = "Agents that imports data from PubChem and populate OntoSpecies"

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless

# Copy Stack-Client resource from published Docker image (potentially ensure
# directory structure matches requirements for jpsrm to update py4jps resources)
COPY --from=stackclients /app ./tmp_stack

# Set the default shell
SHELL ["/bin/bash", "-c"]

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY ./README.md .
COPY ./setup.py .

#------------------------------------
# python setup
#------------------------------------
# Install the required Python libraries
RUN python -m pip install --upgrade pip
RUN python -m pip install -e .
RUN apt-get update && apt-get install -y git

# Update py4jps resources
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Expose port
EXPOSE 5000

#------------------------------------------------------
# Debugging image reusing the base
#------------------------------------------------------
FROM base as debug
# Install additional dependencies for debugging
RUN pip install debugpy

# Set flask app and development environment to allow for hot-reloading
ENV FLASK_APP=app/pubchemagent/flaskapp/wsgi.py
ENV FLASK_DEBUG=1

# Launch application in container
WORKDIR /app/pubchemagent/flaskapp/
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000

#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./pubchemagent ./pubchemagent

# Set the entrypoint
# timeout set to 30min to avoid exceptions for longer API/KG calls
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 pubchemagent.flaskapp.wsgi:app --preload --timeout 7200 

#==================================================================================================