# Install the fenland-trajectory-agent agent in Docker container
#==================================================================================================
FROM ghcr.io/cambridge-cares/stack-client:1.40.1 as stackclients
#------------------------------------------------------
# Base image to be reused
#------------------------------------------------------
FROM python:3.9.14-slim-buster as base

# Keeps Python from generating .pyc files in the container
ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless git

# Copy StackClients resource required to update twa later
RUN mkdir -p ./tmp_stack
COPY --from=stackclients /app ./tmp_stack

# Set the default working directory & copy files to install Python
WORKDIR /app
COPY ./setup.py .
# COPY ./resources/ontop.obda ./resources/ontop.obda
ADD https://jdbc.postgresql.org/download/postgresql-42.5.0.jar ./resources/

# Install Python package and the required libraries
RUN python -m pip install --upgrade pip
RUN pip install -e .
# Installation of agentlogging currently suspended due to performance issues
# Install StackClients twa resource
RUN stack_clients_jar=$(find ../tmp_stack/stack-clients*.jar) && stack_clients_jar=${stack_clients_jar##*/} && \
    jpsrm install StackClients ../tmp_stack --jar $stack_clients_jar

# Expose port
EXPOSE 5000 5678

# #------------------------------------------------------
# # Debugging image reusing the base
# #------------------------------------------------------
FROM base as debug
RUN pip install debugpy
WORKDIR /app/agent
ENV FLASK_APP=/app/agent/app.py
CMD python -m debugpy --listen 0.0.0.0:5678 --wait-for-client -m flask --debug run -h 0.0.0.0 -p 5000


# #------------------------------------------------------
# # Production image reusing the base
# #------------------------------------------------------
FROM base as prod

# # Copy required source code (as code is not attached as volume)
WORKDIR /app/agent
CMD python -m flask run -h 0.0.0.0 -p 5000
# COPY ./agent ./agent
# COPY ./app_entry_point.sh .

# # Set permission 
# RUN chmod +x app_entry_point.sh
# # Set the entrypoint
# ENTRYPOINT ./app_entry_point.sh
# #==================================================================================================

# FROM base as debug
# WORKDIR /agent
# CMD ["flask", "run", "--host=0.0.0.0", "--port=5000", "--debug"]
# # CMD ["sh", "-c", "while true; do sleep 3600; done"]