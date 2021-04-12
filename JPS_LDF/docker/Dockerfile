#########################
#
# This docker file creates an image for the Linked Fragment server
# used in conjunction with the Marie chatbot.
#
# The following stages are included:
# 
#	credentials			Downloads protected files
#	production			Required software only
#
#########################


##### CREDENTIALS STAGE #####
FROM alpine:3.13.4 as credentials

RUN apk update
RUN apk add expect
RUN apk add openssh-client

# Copy in the credentials folder
RUN mkdir -p /usr/src/app 
COPY ./docker/credentials /usr/src/app

# Run download script
WORKDIR /usr/src/app
RUN /usr/bin/expect ./download-hdt.sh


##### PRODUCTION STAGE #####
FROM tomcat:9.0 as production

RUN apt-get update -y 
RUN apt-get install -y unzip maven redis-server
 
# Copy in the files downloaded in last stage
RUN mkdir /usr/src/app
COPY --from=credentials /usr/src/app /usr/src/app
WORKDIR /usr/src/app

# Copy and prepare other files
COPY ./LDF_SERVER_JAVA/Server .
RUN cp -u ./kg/* ./
RUN unzip node_modules.zip -d /usr/src/app

# Set up the LDF and Redis servers
RUN mvn -Dmaven.javadoc.skip=true install
RUN mv ./target/ldfserver-0.2.4.war /usr/local/tomcat/webapps/ldfserver.war

# Install NodeJS
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash
WORKDIR /usr/src/app
COPY ./CACHE_SERVER .
RUN apt-get install -y nodejs --fix-missing
RUN npm install
RUN npm install pm2 -g
 
# Container entrypoint
ENTRYPOINT ["/bin/bash","-c", "redis-server --daemonize yes && /usr/local/tomcat/bin/startup.sh && pm2-runtime comunica_node.js && bash"]
