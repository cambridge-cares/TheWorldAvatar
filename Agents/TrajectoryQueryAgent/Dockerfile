# First stage: retrieve Maven dependencies
#==================================================================================================
FROM maven:3.9-eclipse-temurin-17-alpine AS retriever

# Copy in Maven settings templates and credentials
COPY credentials /root/credentials
COPY .m2 /root/.m2

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

WORKDIR /root/code
COPY trajectoryqueryagent/pom.xml ./pom.xml

RUN --mount=type=cache,id=m2-cache,target=/root/.m2/repository,sharing=locked mvn dependency:resolve

#==================================================================================================
# Second stage: build war file
#==================================================================================================
FROM maven:3.9-eclipse-temurin-17-alpine AS builder

COPY --from=retriever /root/.m2 /root/.m2

WORKDIR /root/code
COPY trajectoryqueryagent /root/code

RUN --mount=type=cache,id=m2-cache,target=/root/.m2/repository,sharing=locked mvn package -DskipTests -U
#==================================================================================================

# Third stage: copy the compiled application into the runtime image
#==================================================================================================
FROM tomcat:9.0 AS agent

WORKDIR /root/trajectoryqueryagent

# Copy the compiled jar from the builder
COPY --from=builder /root/code/output/trajectoryqueryagent##*.war $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

# Port for Java debugging
EXPOSE 5005

ENTRYPOINT ["./entrypoint.sh"]
