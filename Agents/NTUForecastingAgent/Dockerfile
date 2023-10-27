# Reference published Docker image for Stack-Client resources to use
FROM docker.cmclinnovations.com/stack-client:1.6.2 as stackclients

# ---------------------------------------------
# Base image build starts here
# ---------------------------------------------

# Base image is a lightweight version of Python
FROM python:3.9.14-slim-buster AS base

# Install Java and Git and curl
RUN apt update && apt install -y openjdk-11-jdk-headless git && apt-get -y install curl


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
COPY ./NTUForecastingAgent ./NTUForecastingAgent

# Install StackClients py4jps resource
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Install gunicorn for production
RUN pip install gunicorn==20.0.4

# Production image build starts here
FROM base AS production

# Timeout set to 30min to avoid exceptions for longer API/KG calls
CMD gunicorn --bind 0.0.0.0:6000 NTUForecastingAgent:app --timeout 1800
