#==================================================================================================
# Download the Java dependencies (allows them to be cached if unchanged)
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
# Build war file
#==================================================================================================

FROM maven:3.8.3-adoptopenjdk-11 as war_builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/
COPY WEB-INF ./WEB-INF/

RUN mvn package -DskipTests -U

#==================================================================================================
# Build aermet components
#==================================================================================================
FROM ubuntu:22.10 as aermod_builder

RUN apt update
RUN apt install gfortran dos2unix -y

# build AERMOD components
COPY aermet_source /root/aermet_source/
WORKDIR /root/aermet_source
RUN dos2unix ./gfortran-aermod_allwarn.sh
RUN ./gfortran-aermod_allwarn.sh

#==================================================================================================
# Copy compiled executables and war file into tomcat image
#==================================================================================================
FROM tomcat:9 as agent

COPY --from=aermod_builder /root/aermet_source/aermet /root

WORKDIR /app

# Copy the compiled jar from the builder
COPY --from=war_builder /root/code/output/* $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
