# First stage: build war file
#==================================================================================================
FROM maven:3.9.6-eclipse-temurin-17 AS builder

# Copy Maven settings templates and credentials
COPY /credentials /root/credentials
COPY /.m2 /root/.m2

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain /, but not |
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Set work directory
WORKDIR /root/agent

# Copy the pom.xml file
COPY /agent/pom.xml ./pom.xml
# Retrieve all of the dependencies
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository,sharing=locked mvn clean dependency:resolve

# Copy the code
COPY /agent/src/main ./src/main

# Add an option --update-snapshots and argument to ensure the latest versions of SNAPSHOT dependencies are always used
ARG CACHEBUST=1
# Build the war
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository,sharing=locked mvn package -U -DskipTests

#==================================================================================================

# Test stage: create an image for testing based on first stage
#==================================================================================================
FROM builder as test

RUN mkdir -p /root/.m2/repository
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository2,sharing=locked cp -r /root/.m2/repository2/* /root/.m2/repository

COPY /agent/src/test ./src/test

# Execute test
CMD mvn test

#==================================================================================================

# Production stage: copy the downloaded dependency from first stage into a new image and build into an app
#==================================================================================================
FROM tomcat:10.1.24-jre17-temurin AS agent

# Transfer and rename the war file to scheduler-agent as a page route
COPY --from=builder /root/agent/target/*.war $CATALINA_HOME/webapps/scheduler-agent.war
# Expose port
EXPOSE 8080

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]