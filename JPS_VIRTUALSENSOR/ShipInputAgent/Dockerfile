# First stage: download the Java dependencies (allows them to be cached if unchanged)
#==================================================================================================
FROM maven:3.9.3-eclipse-temurin-17 AS retriever

# Copy in Maven settings templates and credentials 
COPY docker/credentials /root/credentials
COPY docker/.m2 /root/.m2

# Populate settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Copy in Java source and build jar
WORKDIR /root/code

COPY pom.xml ./pom.xml
RUN --mount=type=cache,id=aermod-mvn,target=/root/.m2/repository,sharing=locked mvn clean dependency:resolve

#==================================================================================================

# Second stage: build war file
#==================================================================================================

FROM maven:3.9.3-eclipse-temurin-17 AS builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/
COPY WEB-INF ./WEB-INF/

RUN --mount=type=cache,id=airmod-mvn,target=/root/.m2/repository,sharing=locked mvn package -DskipTests -U -P prod-profile

#==================================================================================================

# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9 AS agent

WORKDIR /app

# Copy the compiled jar from the builder
COPY --from=builder /root/code/output/* $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
