# First stage: download java dependencies to allow them to be cached
#==================================================================================================
FROM maven:3.8.6-openjdk-11-slim as retriever

# Copy in Maven settings templates and credentials 
COPY .m2 /root/.m2
COPY ./credentials /root/credentials

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# only download dependencies
COPY FloodAgent/pom.xml /root/pom.xml
WORKDIR /root/
RUN mvn clean dependency:resolve

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM maven:3.8.6-openjdk-11-slim as builder

# copy downloaded maven dependencies
COPY --from=retriever /root/.m2 /root/.m2
# copy Java source files
ADD ./FloodAgent /root/FloodAgent

# Build app
WORKDIR /root/FloodAgent
RUN mvn clean package -DskipTests -U

#==================================================================================================

# Third stage: copy app into a new image
#==================================================================================================
FROM adoptopenjdk/openjdk11:jre-11.0.13_8 as app
WORKDIR /app
# copy compiled jar from previous step
COPY --from=builder /root/FloodAgent/target/*.jar /app
COPY --from=builder /root/FloodAgent/target/lib /app/lib

#==================================================================================================

# default configuration that runs the scheduled updater
# to be executed at CMCL
# docker build --target default -t [TAGNAME] .
#==================================================================================================
FROM app as update
WORKDIR /app
# Run the jar to start the agent
ENTRYPOINT java -jar FloodAgent-1.0.0-SNAPSHOT.jar 

#==================================================================================================

# only writes output files (to be executed on DAFNI)
# docker build --target write-only -t [TAGNAME] .
#==================================================================================================
FROM app as write-only
WORKDIR /app
# Run the jar to start the agent
ENTRYPOINT java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.LaunchWriterOnly

#==================================================================================================