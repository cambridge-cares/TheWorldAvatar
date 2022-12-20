# First stage: download the Java dependencies and build
#==================================================================================================
FROM maven:3.8.3-adoptopenjdk-11 as builder

# Copy in Maven settings templates and credentials 
COPY docker/credentials /root/credentials
COPY docker/.m2 /root/.m2

# Populate settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Copy in Java source and build
WORKDIR /root/code

# Copy just the pom.xml file
COPY pom.xml ./pom.xml

# Retrieve all of the dependencies 
RUN mvn clean dependency:resolve

# Copy in the code
COPY src ./src/

# Ensure that the latest versions of SNAPSHOT dependencies are always used
ARG CACHEBUST=1

RUN mvn deploy --update-snapshots -DskipTests

#==================================================================================================

# Second stage: copy the output into a clean image
#==================================================================================================
FROM adoptopenjdk/openjdk11:jre-11.0.13_8 as agent

# Copy in the entrypoint script
COPY docker/entrypoint.sh /entrypoint.sh

WORKDIR /app

# Copy the downloaded dependencies from the builder
COPY --from=builder /root/code/target/lib /app/lib

# Copy the main jar from the builder
COPY --from=builder /root/code/target/stack-clients*.jar /app

# Port for Java debugging
EXPOSE 5005

# Run the entrypoint script
ENTRYPOINT ["/entrypoint.sh"]
