# Reference published Docker image for Stack-Client resources to use
FROM docker.cmclinnovations.com/stack-client:1.6.2 as stackclients

# ---------------------------------------------
# Base image build starts here
# ---------------------------------------------

# Base image is a lightweight version of Python
FROM python:3.7.11-slim-buster AS base

# Install Java and Git
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy Stack-Client resource from published Docker image
RUN mkdir -p ./tmp_stack
COPY --from=stackclients /app ./tmp_stack

# Set unbuffered mode for Python
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install --upgrade pip
RUN python -m pip install -r requirements.txt

# Set default work directory
WORKDIR /app
COPY ./PVLibAgent ./PVLibAgent

# Install StackClients py4jps resource
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Install gunicorn for production
RUN pip install gunicorn==20.0.4

# Production image build starts here
FROM base AS production

# Copy in properties files for production
COPY ./resources/model_parameters.properties ./PVLibAgent/resources/model_parameters.properties
COPY ./resources/ts_client.properties ./PVLibAgent/kg_utils/resources/ts_client.properties
COPY ./resources/dataIRIs.properties ./PVLibAgent/resources/dataIRIs.properties

CMD gunicorn --bind 0.0.0.0:5000 PVLibAgent:app

# ---------------------------------------------
# Testing image build starts here
# ---------------------------------------------
FROM base AS testing

# Copy tests
COPY ./tests ./tests

# Copy in properties files, including the specific one for testing
COPY ./resources/model_parameters.properties ./PVLibAgent/resources/model_parameters.properties
#COPY ./tests/ts_client_test.properties ./PVLibAgent/kg_utils/resources/ts_client.properties
COPY ./PVLibAgent/kg_utils/resources/ts_client.properties ./PVLibAgent/kg_utils/resources/ts_client.properties
COPY ./resources/dataIRIs.properties ./PVLibAgent/resources/dataIRIs.properties

CMD pytest & gunicorn --bind 0.0.0.0:5000 PVLibAgent:app
