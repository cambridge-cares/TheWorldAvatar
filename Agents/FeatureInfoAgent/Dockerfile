# First stage: build war file
#==================================================================================================
FROM maven:3.8.5-openjdk-17-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2

# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/code
RUN --mount=type=cache,target=/root/.m2/repository mvn package

#==================================================================================================


# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0.69-jdk17 as agent

RUN apt-get update && apt-get install -y nano

WORKDIR /app

COPY --from=builder /root/code/output/feature-info-agent##2.0.0-SNAPSHOT.war $CATALINA_HOME/webapps/
COPY ./tomcat-users.xml /usr/local/tomcat/conf/tomcat-users.xml

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================
