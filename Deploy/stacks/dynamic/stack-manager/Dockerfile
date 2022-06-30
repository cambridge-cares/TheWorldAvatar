#==================================================================================================

# Second stage: build war file
#==================================================================================================

FROM maven:3.8.3-adoptopenjdk-11 as builder

COPY --from=docker.cmclinnovations.com/stack-client-lib:1.0-SNAPSHOT /root/.m2 /root/.m2

# Copy in Java source and build jar
WORKDIR /root/code

COPY pom.xml ./
COPY src ./src/

RUN mvn package

#==================================================================================================

# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM adoptopenjdk/openjdk11:jre-11.0.13_8 as agent

# Copy in the entrypoint script
COPY docker/entrypoint.sh /entrypoint.sh

WORKDIR /app

# Copy the downloaded dependencies from the builder
COPY --from=builder /root/code/target/lib /app/lib

EXPOSE 5005

# Run the entrypoint script
ENTRYPOINT ["/entrypoint.sh"]
