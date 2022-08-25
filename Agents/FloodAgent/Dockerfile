# First stage: download java dependencies to allow them to be cached
#==================================================================================================
FROM maven:3.8.6-openjdk-11-slim as retriever

# Copy in Maven settings templates and credentials 
COPY ./docker/.m2 /root/.m2
COPY ./docker/credentials /root/credentials

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# only download dependencies
COPY ./pom.xml /root/pom.xml
WORKDIR /root/
RUN mvn clean dependency:resolve

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM maven:3.8.6-openjdk-11-slim as builder

# copy downloaded maven dependencies
COPY --from=retriever /root/.m2 /root/.m2
# copy Java source files
ADD ./src /root/FloodAgent/src
ADD ./pom.xml /root/FloodAgent/pom.xml

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
COPY --from=builder /root/FloodAgent/target/classes/ontop.obda /app/classes/ontop.obda

COPY docker/entrypoint.sh /entrypoint.sh
# Port for Java debugging
EXPOSE 5005

ENTRYPOINT ["/entrypoint.sh"]