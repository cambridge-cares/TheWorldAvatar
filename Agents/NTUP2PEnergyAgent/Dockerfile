# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

# Reference published Docker image for Stack-Client resources to use
FROM ghcr.io/cambridge-cares/stack-client:1.23.5 as stackclients

# Base image is a lightweight version of Python
FROM python:3.8-slim-buster

# Install required software
RUN apt update && apt install -y git

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy Stack-Client resource from published Docker image (potentially ensure
# directory structure matches requirements for jpsrm to update py4jps resources)
RUN mkdir -p ./tmp_stack
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
ADD NTUP2PEnergyAgent /app/NTUP2PEnergyAgent

# Copy in config
COPY ./config/buses.py /app/NTUP2PEnergyClusterAgent/config/buses.py

# Install StackClients py4jps resource
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar


# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'python_agent' module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "NTUP2PEnergyAgent:app"]