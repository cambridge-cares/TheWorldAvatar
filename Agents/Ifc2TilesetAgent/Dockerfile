# First stage: Download dependencies and build python code
#==================================================================================================
# Base image is a light weight version of Python
FROM python:3.9-slim as builder

# Install required software including Java
RUN apt update && apt install -y git curl unzip openjdk-11-jdk-headless git

# Expose the port on which our server will run
EXPOSE 5105

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install pip requirements
COPY config/requirements.txt .
RUN python -m pip install -r requirements.txt

# Extract IfcConvert.exe
RUN curl https://s3.amazonaws.com/ifcopenshell-builds/IfcConvert-v0.7.0-cdde536-linux64.zip -o ifcconvert.zip && \
    unzip ifcconvert.zip -d app &&\
    rm -f ifcconvert.zip

# Install ifcopenshell as a Python package
RUN curl https://s3.amazonaws.com/ifcopenshell-builds/ifcopenshell-python-39-v0.7.0-cdde536-linux64.zip -o ifcopenshell.zip && \
    unzip ifcopenshell.zip -d /usr/local/lib/python3.9/site-packages &&\
    rm -f ifcopenshell.zip

# Install npm
RUN curl -fsSL https://deb.nodesource.com/setup_19.x | bash - &&\
    apt install -y nodejs
# Install gltf-pipeline from https://github.com/CesiumGS/gltf-pipeline
RUN npm install -g gltf-pipeline

# Set the default working directory
WORKDIR /app
# Copy the Python source code
COPY ./agent ./agent

#==================================================================================================

# Second stage: Build test image for running unit and integration tests
#==================================================================================================
FROM builder as test

# Install testing frameworks
RUN python -m pip install pytest

# Copy the Python test source code
COPY ./tests ./tests

# Make the required directories for testing
RUN mkdir -p data/ifc data/glb data/gltf && \  
# Execute unit test
pytest

#==================================================================================================

# Third stage: Build production image
#==================================================================================================
FROM builder as prod

# Creates a non-root user and adds permission to access the /app folder
RUN adduser -u 5678 --disabled-password --gecos "" appuser && chown -R appuser /app
USER appuser

# Added timeout of 600s to prevent silent worker killing due to large memory usage
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5105", "agent:app","--timeout", "600"]
#==================================================================================================
