#==================================================================================================
# Download the Java dependencies (allows them to be cached if unchanged)
#==================================================================================================
FROM maven:3.8.3-adoptopenjdk-11 AS retriever

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
RUN --mount=type=cache,id=airmod-mvn,target=/root/.m2/repository,sharing=locked mvn clean dependency:resolve

#==================================================================================================
# Build war file
#==================================================================================================

FROM maven:3.8.3-adoptopenjdk-11 AS war_builder

COPY --from=retriever /root/.m2 /root/.m2

# Copy in Java source and build war
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/
COPY WEB-INF ./WEB-INF/

RUN --mount=type=cache,id=airmod-mvn,target=/root/.m2/repository,sharing=locked mvn package -DskipTests -U -P prod-profile

#==================================================================================================
# Build aermod components
#==================================================================================================
FROM ubuntu:22.10 AS aermod_builder

RUN apt-get update
RUN apt-get install gfortran dos2unix -y

#build BPIPPRM (buildings preprocessor)
COPY bpipprm_source /root/bpipprm_source/
WORKDIR /root/bpipprm_source
RUN dos2unix gfortran-bpipprm.sh
RUN ./gfortran-bpipprm.sh

# build AERMET (meteorological preprocessor)
COPY aermet_source /root/aermet_source/
WORKDIR /root/aermet_source
RUN dos2unix gfortran-aermet.sh
RUN ./gfortran-aermet.sh

# build AERMOD
COPY aermod_source /root/aermod_source/
WORKDIR /root/aermod_source
RUN dos2unix gfortran-aermod.sh
RUN ./gfortran-aermod.sh

#==================================================================================================
# Copy compiled executables and war file into tomcat image
#==================================================================================================
FROM tomcat:9 AS agent

COPY --from=aermod_builder /root/bpipprm_source/bpipprm /root/aermod/
COPY --from=aermod_builder /root/aermet_source/aermet /root/aermod/
COPY --from=aermod_builder /root/aermod_source/aermod /root/aermod/

WORKDIR /app

# Copy the compiled jar from the builder
COPY --from=war_builder /root/code/output/* $CATALINA_HOME/webapps/
COPY ./docker/entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
