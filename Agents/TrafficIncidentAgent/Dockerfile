# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim AS builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
COPY . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml && \
    sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml
# Build
WORKDIR /root/TrafficIncidentAgentCode
RUN --mount=type=cache,target=/root/.m2/repository mvn clean package

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 AS agent

# TODO: check whether WORKDIR needs to be changed to /root/TrafficIncidentAgent
# TODO: check whether needs to ADD https://jdbc.postgresql.org/download/postgresql-42.5.4.jar ./resources/
WORKDIR /app
# # Copy in the properties file
# COPY ./config/api.properties /root/api.properties

# # Set the required environment variable
# ENV TRAFFICINCIDENT_API_PROPERTIES="/root/api.properties"

# #==================================================================================================
# Copy the compiled jar from the builder
COPY --from=builder /root/TrafficIncidentAgentCode/output/traffic-incident-agent##*.war $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

# # Port for Java debugging
EXPOSE 5005

ENTRYPOINT ["./entrypoint.sh"]
#==================================================================================================