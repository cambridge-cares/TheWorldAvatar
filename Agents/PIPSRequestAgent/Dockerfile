# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory, credentials and config files
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/PIPSRequestAgent
RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository,sharing=locked mvn package -D skipTests

#==================================================================================================

# production image stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0.68-jre11-temurin as agent

WORKDIR /app

COPY --from=builder /root/PIPSRequestAgent/output/pips-request-agent##1.0.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================

#==================================================================================================

# test image stage: create an image for unit testing based on common build stage
#==================================================================================================
FROM builder as test

RUN mkdir -p /root/.m2/repository

RUN --mount=type=cache,id=agent-mvn,target=/root/.m2/repository2,sharing=locked cp -r /root/.m2/repository2/* /root/.m2/repository

COPY /PIPSRequestAgent/src/test ./src/test

# Execute test
CMD mvn test
#==================================================================================================