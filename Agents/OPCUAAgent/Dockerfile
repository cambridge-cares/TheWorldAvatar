# Use an official Python runtime as a parent image
FROM python:3.9-slim

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

ENV POSTGRES_CONF /app/OPCUAAgent/config/postgres_conf.properties
ENV OPCUA_CONF /app/OPCUAAgent/config/opcua_conf.properties

# Run command
CMD python ./OPCUAAgent/opcua_agent.py
