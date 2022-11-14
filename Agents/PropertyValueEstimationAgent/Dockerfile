# Install the Average Square Metre Price Agent in Docker container
#==================================================================================================

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster as base
# Meta data
LABEL authors = "mh807@cam.ac.uk"
LABEL description = "Assess average square metre price for instantiated properties in same postcode."

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
ENV FLASK_APP=app/agent/agent:create_app()
ENV FLASK_DEBUG=1

# Launch application in container
WORKDIR /app/agent
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./agent ./agent

# Set the entrypoint
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "agent:create_app()"]


#------------------------------------------------------
# Testing image reusing the base
#------------------------------------------------------
FROM base as test
# Install additional dependencies
RUN pip install gunicorn

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./agent ./agent

# Overwrite/Mock all stack interactions with pre-defined settings from testing Docker stack
COPY ./tests/mockutils/env_configs_mock.py ./agent/utils/env_configs.py
COPY ./tests/mockutils/stack_configs_mock.py ./agent/utils/stack_configs.py
# Adjust relative import statement
RUN sed -i "s|env_configs_mock|env_configs |g" ./agent/utils/stack_configs.py

# Set the entrypoint
# --preload flag is added to provide more information in the container to help with debugging
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "agent:create_app()", "--preload"]

#==================================================================================================