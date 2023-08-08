ARG ONTOP_VERSION=4.1.1
ARG ONTOP_POSTGRES_VERSION=42.2.14.jre7
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

COPY wait-for-it.sh transform_obda.sh ontop.properties.template /opt/ontop/

COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT [ "/entrypoint.sh" ]
#==================================================================================================
