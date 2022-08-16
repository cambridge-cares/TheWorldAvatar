FROM openjdk:8-jre-alpine3.9

WORKDIR /blazegraph

# Download the blazegraph jar
RUN wget -O blazegraph.jar https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_2_1_6_RC/blazegraph.jar

# Expose default Blazegraph port
EXPOSE 9999

# Start blazegraph server
ENTRYPOINT ["java", "-server", "-Xmx4g", "-jar", "blazegraph.jar"]