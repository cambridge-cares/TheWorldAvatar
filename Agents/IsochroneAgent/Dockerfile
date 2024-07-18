# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml


# Build
WORKDIR /root/isochroneagent
RUN --mount=type=cache,target=/root/.m2/repository mvn package

#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /root/isochroneagent


# #==================================================================================================
# Copy the compiled jar from the builder
COPY --from=builder /root/isochroneagent/output/isochroneagent##1.0.0.war $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

EXPOSE 5005


ENTRYPOINT ["./entrypoint.sh"]