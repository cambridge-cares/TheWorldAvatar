#!/bin/bash

# If credentials.properties has been provided as a Docker secret, add it to the war file
credentials_file="/run/secrets/weather_agent_credentials"
if [ -e "$credentials_file" ]; then
  war_file="$CATALINA_HOME/webapps/WeatherAgent.war"

  pushd /tmp
  mkdir -p WEB-INF/classes
  cp "$credentials_file" WEB-INF/classes/credentials.properties
  zip -r "$war_file" WEB-INF/*
  echo "War file updated with new credentials"
fi

# Start Tomcat
catalina.sh run