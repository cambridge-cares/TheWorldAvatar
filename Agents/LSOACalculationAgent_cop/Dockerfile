# Install the lsoa_input agent in Docker container
#==================================================================================================

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.8.10-slim-buster as base
# Meta data
LABEL authors = "jx309@cam.ac.uk"
LABEL description = "LSOA (Lower-layer Super Output Area in the UK) Calculation agent for COP(Coeffcient of performance)"

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy StackClients resource required to update py4jps later
COPY ./tmp_stack ./tmp_stack

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./setup.py .
COPY ./requirements.txt .
COPY ./resources/ontop.obda ./resources/ontop.obda
ADD https://jdbc.postgresql.org/download/postgresql-42.5.0.jar ./resources/

# Install Python package and the required libraries
RUN python -m pip install --upgrade pip
RUN pip install -e .
# Installation of agentlogging currently suspended due to performance issues
RUN pip install -r requirements.txt

# Install StackClients py4jps resource
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
ENV FLASK_APP=app/agent/flaskapp/wsgi.py
ENV FLASK_DEBUG=1

# Launch application in container
WORKDIR /app/agent/flaskapp/
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask run -h 0.0.0.0 -p 5000


#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./agent ./agent
COPY ./app_entry_point.sh .

# Set the entrypoint
ENTRYPOINT ./app_entry_point.sh
#==================================================================================================