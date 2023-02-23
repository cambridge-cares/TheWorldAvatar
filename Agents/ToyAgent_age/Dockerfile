# Install the Property Market Value Estimation Agent in Docker container
#==================================================================================================

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster as base
# Meta data
LABEL authors = "mh807@cam.ac.uk"
LABEL description = "Assess market value estimate for instantiated properties (i.e. buildings, flats)."

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install required software: git and openjdk-11
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy stack-clients resource to be wrapped by py4jps
COPY ./tmp_stack ./tmp_stack

# Set the default working directory & copy files to install Python
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

# Add StackClients py4jps resources
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
ENV FLASK_APP=app/${AGENTDIR}:create_app()
ENV FLASK_DEBUG=1

# Launch application in container
WORKDIR /app/$AGENTDIR
ENTRYPOINT python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./$AGENTDIR ./$AGENTDIR
COPY  ./agent.env.example ./agent.env.example

# Set the entrypoint
# Use shell form to enable variable substitution (https://stackoverflow.com/a/37904830)
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()"


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
ENV FLASK_APP=app/${AGENTDIR}:create_app()
ENV FLASK_DEBUG=1

# Copy docker-compose-file to spin up testcontainers (agent code and tests are mounted as volumes)
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Launch pytest in container (-s flag to enable more output/logging)
WORKDIR /app
# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
#CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000 & pytest -s
CMD python -m debugpy --listen 0.0.0.0:5678 -m flask run -h 0.0.0.0 -p 5000 & pytest -s

#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base as test
# Install additional dependencies
RUN apt-get update
RUN apt install libpq-dev python3-dev gcc -y
RUN python -m pip install -e .[dev]
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./$AGENTDIR ./$AGENTDIR
COPY ./tests ./tests
# Copy docker-compose-file to spin up testcontainers
COPY ./docker-compose-testcontainers.yml ./docker-compose.yml

# Ensure SPARQL endpoints contain 'host.docker.internal' instead of 'localhost' for dockerised agent tests
RUN sed -i "s|localhost|host.docker.internal|g" ./tests/mockutils/env_configs_mock.py
RUN sed -i "s|localhost|host.docker.internal|g" ./tests/mockutils/stack_configs_mock.py
# Ensure flag for Dockerised tests is set to True
RUN sed -i "s|False|True|g" ./tests/mockutils/env_configs_mock.py

# Overwrite/Mock all stack interactions with pre-defined settings from testing Docker stack
RUN cp ./tests/mockutils/env_configs_mock.py ./$AGENTDIR/utils/env_configs.py
RUN cp ./tests/mockutils/stack_configs_mock.py ./$AGENTDIR/utils/stack_configs.py
# Adjust relative import statement
RUN sed -i "s|env_configs_mock|env_configs |g" ./$AGENTDIR/utils/stack_configs.py

# Start dockerised agent app as well as pytest
# (combining commands using & ensures they are started in parallel in background)
CMD gunicorn --bind 0.0.0.0:5000 "${AGENTDIR}:create_app()" & pytest


#==================================================================================================