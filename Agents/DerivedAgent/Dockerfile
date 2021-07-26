# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
#WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
#RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
#RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
# need to manually copy the entire folder for JPS_BASE_LIB here for now, until a proper versioning is in place
WORKDIR /root/JPS_BASE_LIB
RUN mvn clean install -DskipTests

WORKDIR /root/DerivedAgent
RUN mvn clean install
#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

COPY --from=builder /root/DerivedAgent/output/DerivedAgent##1.0.0-SNAPSHOT.war $CATALINA_HOME/webapps/

# these settings are only required for debug mode
ENV JPDA_ADDRESS=0.0.0.0:8000
ENV JPDA_TRANSPORT=dt_socket
EXPOSE 8000

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "jpda", "run"]
#==================================================================================================