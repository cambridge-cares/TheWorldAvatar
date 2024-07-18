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
WORKDIR /root/RFIDQueryAgentCode
RUN --mount=type=cache,target=/root/.m2/repository mvn clean package

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

# Copy in the properties file
COPY ./config/client.properties /app/config/client.properties

# Copy in the second properties file
COPY ./config/species.properties /app/config/species.properties

# Set the required environment variable for ts client properties
ENV CLIENT_PROPERTIES="/app/config/client.properties"

# Set the required environment variable for species properties
ENV SPECIES_PROPERTIES="/app/config/species.properties"

# Set the environment variable that points to where the email agent is located at
ENV EMAIL_AGENT_URL="http://host.docker.internal:8080/email_agent"

COPY --from=builder /root/RFIDQueryAgentCode/output/rfid-query-agent##1.2.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================