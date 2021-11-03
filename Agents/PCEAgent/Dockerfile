# Artifact ID is defined globally so that it can be used in both stages
ARG artifact_id="trained_models"

# First stage: download maven artifact
#==================================================================================================
# N.B. This version of the official maven docker image is used because newer versions (e.g. maven:3.8.1-adoptopenjdk-11)
# seem to be incompatible with CMCL's nexus-based maven repositories
# At least one earlier version (maven:3.5.0-jdk-9) fails with certificate errors.
FROM maven:3.6-openjdk-11-slim as dependency_fetcher

# Specify repository and artifact details
ARG repo_url="https://maven.pkg.github.com/ucam-ceb-como/oscml/"
ARG group_id="com.oscml"
ARG artifact_fmt="zip"
ARG artifact_version="1.0.0-SNAPSHOT"
# artifact_id has to be declared here so that the globally-defined value is available in this stage
ARG artifact_id

# Copy all files into root's home, including the pom file and ./m2 directory
ADD retrieved_models /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_URL|${repo_url}|g;s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Populate repository, artifact details in the pom
WORKDIR /root/
RUN sed -i "s|REPO_URL|${repo_url}|g;s|GROUP_ID|${group_id}|;s|ARTIFACT_ID|${artifact_id}|g;s|ARTIFACT_VERSION|${artifact_version}|;s|ARTIFACT_FMT|${artifact_fmt}|" pom.xml

# Download dependencies as described in the pom
RUN mvn -f pom.xml org.apache.maven.plugins:maven-dependency-plugin:unpack-dependencies
#==================================================================================================

# Second stage: install the oscml app
#==================================================================================================
FROM continuumio/miniconda3 as oscml_app

# Set the artifact_id again otherwise it will be blank in this stage
ARG artifact_id
# Expose the port on which our server will run
EXPOSE 5000
# Keeps Python from generating .pyc files in the container
#ENV PYTHONDONTWRITEBYTECODE=1
# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1
SHELL ["/bin/bash", "-c"]
# Set the default working directory, then copy the Python source code into it
WORKDIR /app
COPY ./oscml /app/oscml/
COPY ./LICENSE /app/.
COPY ./README.md /app/.
COPY ./setup.py /app/.
COPY ./cronconfig /app/.
COPY ./environment_cpu.yml /app/.
COPY ./environment_gpu.yml /app/.
COPY ./delete_logs.sh /app/.
COPY ./app_entry_point.sh /app/.
COPY ./install_script.sh /app/.
COPY --from=dependency_fetcher /root/${artifact_id} ./retrieved_models/${artifact_id}

#------------------------------------
# cron setup
#------------------------------------
# Get cron
RUN apt-get update && apt-get -y install cron
# Setup cron job
ADD ./cronconfig /etc/cron.d/cronconfig
# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/cronconfig
# Give execution rights to the script
RUN chmod 0744 /app/delete_logs.sh
# Apply cron job
RUN crontab /etc/cron.d/cronconfig
# Create the log file to be able to run tail
RUN touch /var/log/cron.log

#------------------------------------
# conda setup
#------------------------------------
ARG conda_env=oscml_venv
# Install the required Python libraries
RUN ./install_script.sh -v -n $conda_env -i -e
# Make RUN commands use the new environment:
RUN echo "source activate $conda_env" > ~/.bashrc
ENV PATH /opt/conda/envs/$conda_env/bin:$PATH
RUN conda install openjdk

#------------------------------------
# entry point setup
#------------------------------------
# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
#RUN useradd appuser && chown -R appuser /app
#USER appuser
# Set the entrypoint
RUN chmod 0744 /app/app_entry_point.sh
ENTRYPOINT /app/app_entry_point.sh
#==================================================================================================