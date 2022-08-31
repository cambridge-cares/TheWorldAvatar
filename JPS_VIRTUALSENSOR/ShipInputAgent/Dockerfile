# First stage: download the Java dependencies (allows them to be cached if unchanged)
#==================================================================================================
FROM maven:3.8.3-adoptopenjdk-11 as retriever

# Copy in Maven settings templates and credentials 
COPY docker/credentials /root/credentials
COPY docker/.m2 /root/.m2

# Populate settings templates with credentials
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Copy in Java source and build jar
WORKDIR /root/code

COPY pom.xml ./pom.xml
RUN mvn clean dependency:resolve

#==================================================================================================

# Second stage: build war file
#==================================================================================================

FROM maven:3.8.3-adoptopenjdk-11 as builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/

RUN mvn package -DskipTests -U

#==================================================================================================

# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM adoptopenjdk/openjdk11:jre-11.0.13_8 as app

WORKDIR /app

# copy compiled jar from previous step
COPY --from=builder /root/code/target/*.jar /app
COPY --from=builder /root/code/target/lib /app/lib
COPY ./docker/entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
