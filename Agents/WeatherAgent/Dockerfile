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
WORKDIR /root/WeatherAgent
RUN --mount=type=cache,target=/root/.m2/repository mvn clean install -DskipTests
#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

# Install zip so that the entrypoint script can modify the war at runtime, if necessary
RUN apt update && apt install -y zip

# Remove version suffix when copying the war to simplify the entrypoint script
COPY --from=builder /root/WeatherAgent/output/WeatherAgent##1.0.0-SNAPSHOT.war $CATALINA_HOME/webapps/WeatherAgent.war

COPY ./docker/entrypoint.sh .

# Run the entrypoint script
ENTRYPOINT ["./entrypoint.sh"]
#==================================================================================================