# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Allow Maven profile to be passed as an arg, defaulting to 'dev-profile'
ARG mvn_profile="dev-profile"

WORKDIR /root
COPY docker .

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/build

# Copy in source files ./m2 directory and credentials
COPY ./FileServerAgent .

RUN --mount=type=cache,target=/root/.m2/repository mvn package -P $mvn_profile
#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

COPY --from=builder /root/build/output/FileServerAgent##1.0.0-SNAPSHOT.war $CATALINA_HOME/webapps/

# Overwrite tomcat-users to set up a role for authentication
COPY ./docker/tomcat/conf/tomcat-users.xml /usr/local/tomcat/conf/

# Create a directory which will act as the file server root, to be mounted to a voume
WORKDIR /app/fs_root

# Copy-in entrypoint script
COPY ./docker/entrypoint.sh /app/

ENTRYPOINT ["/app/entrypoint.sh"]
#==================================================================================================