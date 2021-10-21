ARG ONTOP_VERSION
ARG ONTOP_POSTGRES_VERSION
# First stage: Retrieve postgres jar file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as jar_fetcher

ARG ONTOP_POSTGRES_VERSION

RUN mvn org.apache.maven.plugins:maven-dependency-plugin:2.1:get \
    -DrepoUrl=https://mvnrepository.com/artifact/org.postgresql/postgresql \
    -Dartifact=org.postgresql:postgresql:${ONTOP_POSTGRES_VERSION}
#==================================================================================================

# Second stage: copy jar into Ontop image
#==================================================================================================
FROM ontop/ontop-endpoint:${ONTOP_VERSION}

ARG ONTOP_POSTGRES_VERSION

COPY --from=jar_fetcher \
     /root/.m2/repository/org/postgresql/postgresql/${ONTOP_POSTGRES_VERSION}/postgresql-${ONTOP_POSTGRES_VERSION}.jar \
     /opt/ontop/jdbc/

COPY obda . /opt/ontop/

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
#==================================================================================================