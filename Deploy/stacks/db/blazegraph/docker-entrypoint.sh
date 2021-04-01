#!/bin/bash

# Entrypoint script that updates some configuration files for the server and workbench before running tomcat


# Update the contents of WEB-INF in the blazegraph war file
pushd /tmp/blazegraph
#zip /usr/local/tomcat/webapps/blazegraph.war WEB-INF/web.xml
zip -r /usr/local/tomcat/webapps/blazegraph.war WEB-INF/*
popd

# Set password in the tomcat-users.xml using docker secret values
sed -i "s/BLAZEGRAPH_PASSWORD/$(cat $BLAZEGRAPH_PASSWORD_FILE)/" /usr/local/tomcat/conf/tomcat-users.xml

# Start tomcat (deploys modified war file)
catalina.sh run
