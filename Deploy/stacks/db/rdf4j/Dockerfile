FROM docker.cmclinnovations.com/eclipse/rdf4j-workbench:amd64-3.5.0
EXPOSE 8080

# Install zip in order to modify .war files (need to be root)
USER root
RUN apk update && apk add zip

# Overwrite tomcat-users.xml to add 'rdf4j-admin' and 'rdf4j-user' roles
COPY tomcat/conf/tomcat-users.xml /usr/local/tomcat/conf/tomcat-users.xml

COPY tomcat/conf/server.xml /usr/local/tomcat/conf/server.xml

# Configuration files for the server and workbench can't just be copied in, as they'd get
# overwritten when the war files are deployed. Instead, copy them to a temporary location and use
# an entrypoint script to add them to the war files before tomcat is started.
COPY rdf4j-server_war /tmp/rdf4j-server
COPY rdf4j-workbench_war /tmp/rdf4j-workbench

# Copy in script and set it as the entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]

