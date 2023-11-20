# Install the District Heating Optimisation Trigger Agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use
FROM ghcr.io/cambridge-cares/stack-client:1.20.1 as stackclients

FROM python:3.9.14-slim-buster
# Meta data
LABEL authors = "mh807@cam.ac.uk"
LABEL description = "District Heating Optimisation Trigger Agent"~

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git
# Install Curl (required to test whether instantiation container has finished)
RUN apt install -y curl

# Install Redis for scheduling of background tasks using Celery (via Flask)
RUN apt install -y redis-server

# Copy Stack-Client resource from published Docker image
COPY --from=stackclients /app ./tmp_stack

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./requirements.txt .
COPY ./setup.py .

# Install Python package and the required libraries 
# (use pinned versions from requirements.txt to avoid potential clashes before installing package itself)
RUN python -m pip install --upgrade pip
RUN python -m pip install -r requirements.txt
RUN python -m pip install -e .

# Update py4jps resources
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Expose port
EXPOSE 5000

# Copy required source code and resources
COPY ./agent ./agent
COPY ./resources ./resources
COPY ./delayed_start.sh .

# Start the Celery worker and Gunicorn server
# Create pipeline of multiple commands using Shell Form of CMD (to perform a series of operations)
# 1) Start Redis server at default 'localhost:6379'
# 2) Point the celery worker to the celery_app object
#    NOTE: Although not recommended, celery is run as superuser to avoid permission issues
# 3) Serve Flask App via Gunicorn
#    NOTE: Infinite timeout to avoid exceptions for longer (recurring) optimisations

CMD redis-server & \
    celery -A agent.celery worker --loglevel=info & \
    gunicorn -b 0.0.0.0:5000 agent:app --timeout 0

#==================================================================================================