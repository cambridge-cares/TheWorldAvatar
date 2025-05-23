# First stage: build war file for main agent
#==================================================================================================
FROM maven:3.8.6-eclipse-temurin-11-focal as builder

# Copy Maven settings templates and credentials
COPY /credentials /root/credentials
COPY /.m2 /root/.m2

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain /, but not |
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml &&  \
    sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Set work directory
WORKDIR /root/IfcOwlConverter

# Copy the pom.xml file
COPY /IfcOwlConverter/pom.xml ./pom.xml
# Retrieve all of the dependencies
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository,sharing=locked mvn clean dependency:resolve

# Copy the code
COPY /IfcOwlConverter/src/main ./src/main

# Build the war
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository,sharing=locked mvn package -DskipTests


#==================================================================================================

# Second stage: create an image for unit testing based on first stage
#==================================================================================================
FROM builder as test

RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository2,sharing=locked cp -r /root/.m2/repository2/* /root/.m2/repository

COPY /IfcOwlConverter/src/test ./src/test

# Execute test
RUN mvn test

#==================================================================================================

# Third stage: copy the downloaded dependency from first stage into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0.68-jre11-temurin as agent

# Copies war file from build directory to destination
COPY --from=builder /root/IfcOwlConverter/output/ifcowlconverter##1.0.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================
