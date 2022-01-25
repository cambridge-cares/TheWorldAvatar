# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-8-slim as builder

# Copy all files into root's home
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/FloodAgent
RUN mvn clean install -DskipTests

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM openjdk:8-jre-alpine3.9 as agent

WORKDIR /app

# Copy the compiled jar from the builder
COPY --from=builder /root/FloodAgent/target/*.jar /app
# Copy the downloaded dependencies from the builder
COPY --from=builder /root/FloodAgent/target/lib /app/lib
RUN apk update && apk add dos2unix bash

#==================================================================================================

# default configuration that runs the scheduled updater and writes output files
# to be executed at CMCL
# docker build --target default -t [TAGNAME] .
#==================================================================================================
FROM agent as default
# Copy in start up script
# Install web server (to host json files for download)
RUN apk add apache2
COPY ./httpd.conf /etc/apache2/httpd.conf
RUN mkdir -p /var/www/html/
RUN chown -R apache:apache /var/www/
RUN chmod -R 775 /var/www/

COPY ./start-up.sh /app/start-up.sh
RUN dos2unix /app/start-up.sh
RUN chmod -R 775 /app/start-up.sh

# Run the jar to start the agent
ENTRYPOINT bash /app/start-up.sh

#==================================================================================================

# only writes output files (to be executed on DAFNI)
# docker build --target write-only -t [TAGNAME] .
#==================================================================================================
FROM agent as write-only
# Copy in start up script
COPY ./start-up-writer-only.sh /app/start-up-writer-only.sh
RUN dos2unix /app/start-up-writer-only.sh
RUN chmod -R 775 /app/start-up-writer-only.sh

# Run the jar to start the agent
ENTRYPOINT bash /app/start-up-writer-only.sh

#==================================================================================================