FROM docker.cmclinnovations.com/tomcat:9.0

# Arguments
ARG HASH

# Labels
LABEL authors = "support@cmclinnovations.com"
LABEL description = "Blazegraph with BASIC authentication, based on the official Tomcat 9 image."
LABEL hash = $HASH

# Download Blazegraph .war file
RUN wget -O /usr/local/tomcat/webapps/blazegraph.war https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_2_1_6_RC/blazegraph.war

# Install zip in order to modify .war file
RUN apt-get update && apt-get install zip && rm -rf /var/lib/apt/lists/*

# Overwrite tomcat-users.xml to add blazegraph role
COPY tomcat/conf/tomcat-users.xml /usr/local/tomcat/conf/tomcat-users.xml

# Configuration files can't just be copied in, as they'd get overwritten when the war file is
# deployed. Instead, copy them to a temporary location and use an entrypoint script to add them to
# the war file before tomcat is started.
COPY blazegraph_war /tmp/blazegraph

# Copy in script and set it as the entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

