# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/esphome-agent-code
RUN mvn package

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

# Copy in the properties file
COPY ./config/api.properties /root/api.properties

# Copy in the properties file
COPY ./config/client.properties /root/client.properties

# Copy in the properties file
COPY ./config/esphome-client.properties /root/esphome-client.properties

# Set the required environment variable
ENV API_PROPERTIES="/root/api.properties"

# Set the required environment variable
ENV CLIENT_PROPERTIES="/root/client.properties"

# Set the required environment variable
ENV ESPHOME_CLIENT_PROPERTIES="/root/esphome-client.properties"

#Set the required environment variable
ENV ACCESS_AGENT_TARGETRESOURCEID="http://host.docker.internal:48888/esphome"

COPY --from=builder /root/esphome-agent-code/output/esphome-agent##1.4.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================