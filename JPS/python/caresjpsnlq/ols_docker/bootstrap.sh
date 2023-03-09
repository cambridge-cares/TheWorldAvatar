#!/bin/bash

#start mongodb 
mongod --fork --logpath /var/log/mongodb.log \

#start SOLR
/opt/solr/bin/solr -Dsolr.solr.home=/opt/OLS/ols-solr/src/main/solr-5-config/ -Dsolr.data.dir=/opt/OLS

# Start Tomcat7
cd /etc/init.d/
./tomcat7 start

# this script must end with a persistent foreground process
tail -f /var/lib/tomcat7/logs/catalina.out
