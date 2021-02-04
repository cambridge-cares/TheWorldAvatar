#!/bin/bash

# Entrypoint script that updates some configuration files for the server and workbench before running tomcat


# Update web.xml in the rdf4j-server war file
pushd /tmp/rdf4j-server
zip /usr/local/tomcat/webapps/rdf4j-server.war WEB-INF/web.xml
popd

# Update web.xml in the rdf4j-workbench war file
pushd /tmp/rdf4j-workbench
zip /usr/local/tomcat/webapps/rdf4j-workbench.war WEB-INF/web.xml
popd

# Set passwords in the tomcat-users.xml using docker secret values
sed -i "s/RDF4J_ADMIN_PASSWORD/$(cat $RDF4J_ADMIN_PASSWORD_FILE)/" /usr/local/tomcat/conf/tomcat-users.xml
sed -i "s/RDF4J_USER_PASSWORD/$(cat $RDF4J_USER_PASSWORD_FILE)/" /usr/local/tomcat/conf/tomcat-users.xml

# Start tomcat (deploys modified war files)
catalina.sh run
