# Use an official Python runtime as a parent image
FROM python:3.9-slim AS base

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install --upgrade pip
RUN python -m pip install -r requirements.txt

# Set the working directory in the container
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY ./OPCUAAgent ./OPCUAAgent

ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1
# Set the PYTHONPATH environment variable
ENV PYTHONPATH=/app

FROM base AS production

ENV POSTGRES_CONF /app/OPCUAAgent/config/postgres_conf.properties
ENV OPCUA_CONF /app/OPCUAAgent/config/opcua_conf.properties

# Run command
CMD python ./OPCUAAgent/opcua_agent.py

# ---------------------------------------------
# Testing image build starts here
# ---------------------------------------------
FROM base AS test_opcua_agent

# Copy tests
COPY ./tests ./tests
    
CMD pytest --log-cli-level=INFO