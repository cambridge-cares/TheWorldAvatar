# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python
# Reference published Docker image for Stack-Client resources to use
FROM docker.cmclinnovations.com/stack-client:1.6.2 as stackclients

FROM osgeo/gdal:latest

USER root

# Install Java
#RUN apt update && apt install -y openjdk-11-jdk-headless git gcc libgl1-mesa-glx
#RUN apt update && apt install -y openjdk-11-jdk-headless git gcc libgl1-mesa-glx g++
RUN apt update && apt install -y pip git

# Copy Stack-Client resource from published Docker image (potentially ensure
# directory structure matches requirements for jpsrm to update py4jps resources)
COPY --from=stackclients /app ./tmp_stack

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install --upgrade pip
RUN python -m pip install -r requirements.txt


# Set the default working directory, then copy the Python source code into it
WORKDIR /app
ADD GeoSegmentAgent /app/GeoSegmentAgent
# Copy raster files in the raster_input directory
COPY GeoSegmentAgent/raster_input /app/GeoSegmentAgent/raster_input

# Install StackClients py4jps resource
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar


ENTRYPOINT ["gunicorn", "--preload", "--bind", "0.0.0.0:5000", "GeoSegmentAgent:app", "--timeout", "3600"]