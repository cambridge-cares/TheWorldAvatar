# This Dockerfile creates an image for the Linked Data Fragments Server used by JPS_Chatbot (Marie)

# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy in the .m2 directory and credentials
ADD docker /root

# Populate the settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Copy in the Java source and build the .war file
WORKDIR /root/build
COPY components/LDF_SERVER_JAVA/Server .
RUN mvn -Dmaven.test.skip=true -Dmaven.javadoc.skip=true package -U

# Unpack other dependencies
RUN mvn org.apache.maven.plugins:maven-dependency-plugin:unpack-dependencies -U
#==================================================================================================

# Second stage - copy in dependencies, run war file in Tomcat,
#==================================================================================================
FROM tomcat:9.0.46-jdk11-openjdk-slim-buster

# Install prequisites: nodeJS, pm2 and redis-server
RUN apt-get update && apt-get install -y curl npm redis-server
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash
RUN apt-get install nodejs -y --fix-missing
RUN npm install && npm install pm2 -g

WORKDIR /usr/src/app

# Copy in the entrypoint script
COPY entrypoint.sh .

# Copy in dependencies from the first stage
COPY --from=builder /root/build/dependencies ./
# Copy in .war file from the first stage
COPY --from=builder /root/build/target/ldfserver.war /usr/local/tomcat/webapps/

# Copy in source files for the Cache server
COPY components/CACHE_SERVER .

# Entrypoint script starts redis, tomcat, node app
ENTRYPOINT ["./entrypoint.sh"]