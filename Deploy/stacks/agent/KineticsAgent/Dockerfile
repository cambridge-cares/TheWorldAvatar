#########################
#
# This docker file creates an Image with an environment
# for running the KineticsAgent within a Tomcat server.
# 
# The "docker build" command used to build this file
# into a Image should be run from the docker directory.
# See the README for more details.
#
# NOTE: This Image requires that users have prebuilt
# the KineicsAgent.war on the host machine and copied it into 
# this directory BEFORE attempting to build this image.
# As the war file has to be built with the correct SSH credentials
# listed in it's properties file each time, it's not really 
# possible to ask CARES developers to upload it to Nexus and
# download it here.
#
#########################

# Use the DockerHub version of tomcat as the base
FROM tomcat:jdk8-corretto as base

# Update package repository
RUN yum update -y
ENV JAVA_HOME="/etc/alternatives/java_sdk"

# Install Maven
RUN echo "Installing Maven..."
RUN yum install -y maven
ENV M2_HOME="/usr/share/maven"
ENV MAVEN_HOME="/usr/share/maven"
RUN echo "Maven installed and configured."

# Copy across Maven files
RUN mkdir /tmp/setup
WORKDIR /tmp/setup
COPY pom.xml pom.xml
COPY settings.xml settings.xml

# Run Maven project to download resources
RUN mvn -X install -s settings.xml

# Put SRM SimDOME files in a directory
RUN mkdir /usr/local/simdome
RUN mv ./output/simdome/* /usr/local/simdome/
RUN chmod -R 775 /usr/local/simdome

# Setup a non-root user to run tomcat
RUN yum install -y shadow-utils
RUN yum install -y procps

# Copy in the KineticsAgent WAR
COPY KineticsAgent.war /usr/local/tomcat/webapps/

# Copy in the tomcat server settings
COPY server.xml /usr/local/tomcat/conf/server.xml

# Create tomcat user and group
RUN adduser tomcat
RUN groupadd tomcats
RUN usermod -a -G tomcats tomcat

# Set permissions
RUN chown -R tomcat:tomcats /usr/local/tomcat
RUN chown -R tomcat:tomcats /usr/local/simdome
USER tomcat

# Expose the port listed in the settings
EXPOSE 8080

# Java and Maven cleanup
USER root
RUN rm -rf ./target
RUN rm -f pom.xml settings.xml
RUN yum remove -y maven
RUN unset M2_HOME
RUN unset MAVEN_HOME