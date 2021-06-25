# Artifact ID is defined globally so that it can be used in both stages
ARG artifact_id="model_files"

# First stage: download maven artifact
#==================================================================================================
# N.B. This version of the official maven docker image is used because newer versions (e.g. maven:3.8.1-adoptopenjdk-11)
# seem to be incompatible with CMCL's nexus-based maven repositories
# At least one earlier version (maven:3.5.0-jdk-9) fails with certificate errors.
FROM maven:3.6-openjdk-11-slim as dependency_fetcher

# Specify repository and artifact details
ARG repo_url="https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/"
ARG group_id="com.example"
ARG artifact_fmt="zip"
ARG artifact_version="1.0.0"
# artifact_id has to be declared here so that the globally-defined value is available in this stage
ARG artifact_id

# Copy all files into root's home, including the pom file and ./m2 directory
ADD docker /root

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

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM python:3.8-slim-buster as app

# artifact_id has to be declared here so that the globally-defined value is available in this stage
ARG artifact_id

WORKDIR /app

COPY --from=dependency_fetcher /root/${artifact_id} ./${artifact_id}

# Set up the rest of your app here, e.g.
# ADD python_app .
# ENTRYPOINT ["python", "./app.py"]

# Dummy entrypoint to prevent the container from exiting
ENTRYPOINT ["tail", "-f", "/dev/null"]
#==================================================================================================