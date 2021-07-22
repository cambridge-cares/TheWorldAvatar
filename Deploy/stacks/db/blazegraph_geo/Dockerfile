# First stage: download war
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as dependency_fetcher

# Specify repository and artifact details
ARG repo_url="https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/"
ARG group_id="uk.ac.cam.cares.jps.tstore.blazegraph-geo"
ARG artifact_fmt="zip"
ARG artifact_id="blazegraph_war"
ARG artifact_version="1.0.1"

# Copy Maven config files and credentials into root's home
ADD docker /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_URL|${repo_url}|g;s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Populate repository, artifact details in the pom
WORKDIR /root/
RUN sed -i "s|REPO_URL|${repo_url}|g;s|GROUP_ID|${group_id}|;s|ARTIFACT_ID|${artifact_id}|g;s|ARTIFACT_VERSION|${artifact_version}|;s|ARTIFACT_FMT|${artifact_fmt}|" pom.xml

# Download war
RUN mvn -f pom.xml org.apache.maven.plugins:maven-dependency-plugin:unpack-dependencies
#==================================================================================================

# Second stage: copy the downloaded war into the the webapps dir of a tomcat image
#==================================================================================================
FROM docker.cmclinnovations.com/tomcat:9.0 as blazegraph_geo

# Copy in local,customised version of the Blazegraph .war file
COPY --from=dependency_fetcher /root/blazegraph.war /usr/local/tomcat/webapps