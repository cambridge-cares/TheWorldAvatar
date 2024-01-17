#!/bin/bash

# Entrypoint script that updates some configuration files for the server and workbench before running tomcat


# Update the contents of WEB-INF in the blazegraph war file
pushd /tmp/blazegraph || exit

if [[ -n $BLAZEGRAPH_USER && -n $BLAZEGRAPH_PASSWORD_FILE && -f $BLAZEGRAPH_PASSWORD_FILE ]]; then
    # Set username and password in the tomcat-users.xml using docker secret values
    sed -i -e "s/BLAZEGRAPH_USER/$BLAZEGRAPH_USER/" -e "s/BLAZEGRAPH_PASSWORD/$(cat $BLAZEGRAPH_PASSWORD_FILE)/" /usr/local/tomcat/conf/tomcat-users.xml
else
    # Remove "security-constraint" related blocks
    sed -i -e '/<security-constraint>/,/<\/security-constraint>/d' -e '/<login-config>/,/<\/login-config>/d' -e '/<security-role>/,/<\/security-role>/d' WEB-INF/web.xml
fi

#zip /usr/local/tomcat/webapps/blazegraph.war WEB-INF/web.xml
zip -r /usr/local/tomcat/webapps/blazegraph.war WEB-INF/*
popd || exit

# Start tomcat (deploys modified war file)
catalina.sh run

