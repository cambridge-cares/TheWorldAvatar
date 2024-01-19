# First stage: download dependencies
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as retriever

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
COPY ./pom.xml /root
COPY ./docker/credentials /root/credentials
COPY ./docker/.m2 /root/.m2

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/
RUN --mount=type=cache,id=aermod-mvn,target=/root/.m2/repository,sharing=locked mvn clean dependency:resolve

#==================================================================================================

# Second stage: build war file
#==================================================================================================
FROM maven:3.8.3-adoptopenjdk-11 as builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY ./pom.xml ./
COPY ./src ./src/
COPY ./WEB-INF ./WEB-INF/

RUN --mount=type=cache,id=aermod-mvn,target=/root/.m2/repository,sharing=locked mvn package -DskipTests -U

#==================================================================================================

# third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

COPY --from=builder /root/code/output/* $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]