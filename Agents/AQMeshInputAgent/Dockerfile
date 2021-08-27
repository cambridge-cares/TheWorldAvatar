# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-8-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory, credentials and config files
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/AQMeshInputAgent
RUN mvn package

# Set API username and password from credentials
RUN sed -i "s|USERNAME|$(sed -n 1p ../credentials/api_credentials.txt)|;s|PASSWORD|$(sed -n 2p ../credentials/api_credentials.txt)|" ../config/api.properties
# Set postgres username and password from credentials
RUN sed -i "s|USERNAME|$(sed -n 1p ../credentials/postgres_credentials.txt)|;s|PASSWORD|$(sed -n 2p ../credentials/postgres_credentials.txt)|" ../config/client.properties

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM openjdk:8-jre-alpine3.9 as agent

WORKDIR /app

# Copy configurations needed to run the agent from the builder
COPY --from=builder /root/config/*.properties /app/config/
# Copy the compiled jar from the builder
COPY --from=builder /root/AQMeshInputAgent/target/aqmesh_inputAgent-1.0.0-SNAPSHOT.jar /app
# Copy the downloaded dependencies from the builder
COPY --from=builder /root/AQMeshInputAgent/target/aqmesh_inputAgent-1.0.0-SNAPSHOT.lib /app/aqmesh_inputAgent-1.0.0-SNAPSHOT.lib

# Run the jar to start the agent
ENTRYPOINT ["java", "-jar", "aqmesh_inputAgent-1.0.0-SNAPSHOT.jar", "config/agent.properties", "config/client.properties", "config/api.properties"]
#==================================================================================================