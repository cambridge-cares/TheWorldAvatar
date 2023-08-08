# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|WA_REPO_USERNAME|$(cat ../credentials/wa_repo_username.txt)|;s|WA_REPO_PASSWORD|$(cat ../credentials/wa_repo_password.txt|xargs mvn --encrypt-password)|;s|CMCL_REPO_USERNAME|$(cat ../credentials/cmcl_repo_username.txt)|;s|CMCL_REPO_PASSWORD|$(cat ../credentials/cmcl_repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/MoDSMooAgent
RUN --mount=type=cache,target=/root/.m2/repository mvn package
#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

COPY --from=builder /root/MoDSMooAgent/output/webapps/MoDSMooAgent.war $CATALINA_HOME/webapps/

ENV JPDA_ADDRESS=0.0.0.0:8000
ENV JPDA_TRANSPORT=dt_socket
EXPOSE 8000

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "jpda", "run"]

#==================================================================================================