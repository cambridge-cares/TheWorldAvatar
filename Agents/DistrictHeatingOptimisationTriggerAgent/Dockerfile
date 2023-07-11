# Install the District Heating Optimisation Trigger Agent in Docker container
#==================================================================================================
# Reference published Docker image for Stack-Client resources to use
FROM docker.cmclinnovations.com/stack-client:1.6.2 as stackclients

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

# Copy required source code (as code is not attached as volume)
COPY ./agent ./agent

# Set the entrypoint
# Infinite timeout to avoid exceptions for longer (recurring) optimisations
ENTRYPOINT gunicorn --bind 0.0.0.0:5000 "agent:create_app()" --timeout 0

#==================================================================================================