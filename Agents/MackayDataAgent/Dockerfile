# Install the Forecasting Agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use

#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster as base
# Meta data
LABEL authors = "sz375@cam.ac.uk"
LABEL description = "Mackay Data Agent"

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1
# Disable pip cache to keep image size as small as possible
ENV PIP_NO_CACHE_DIR=false

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy Stack-Client resource from published Docker image
#COPY --from=stackclients /app ./tmp_stack

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./README.md .
COPY ./requirements.txt .

# Install Python package and the required libraries
# (use pinned versions from requirements.txt to avoid potential clashes before installing package itself)
RUN python -m pip install --upgrade pip
#RUN apt-get install -y libpq-dev python-dev
RUN python -m pip install -r requirements.txt
#RUN python -m pip install -e .

# Update py4jps resources
#RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
#    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Expose port
EXPOSE 5000

#------------------------------------------------------
# Production image reusing the base
#------------------------------------------------------
FROM base as prod
# Install additional dependencies
RUN pip install gunicorn==20.0.4

# Copy required source code (as code is not attached as volume)
WORKDIR /app
COPY ./mackay_data_agent ./mackay_data_agent
COPY ./confs_files ./confs_files
COPY ./data_classes ./data_classes
COPY ./kg_access ./kg_access
COPY ./tbox_dev ./tbox_dev
COPY ./utils ./utils
COPY ./tests ./tests

COPY app.py ./app.py
# Set the entrypoint
# timeout set to 30min to avoid exceptions for longer API/KG calls
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "app:create_app()" --timeout 1800