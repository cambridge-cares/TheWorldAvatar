#!/bin/bash

# Set the password used for BASIC authentication
secret_file="/run/secrets/file_server_password"
if [ -e "$secret_file" ]; then
    fs_password=$(cat "$secret_file")
else
    # If password wasn't provided as a secret, set a default
    fs_password="fs_pass"
fi

# Write the password to tomcat-users.xml
sed -i "s/FS_PASSWORD/$fs_password/" /usr/local/tomcat/conf/tomcat-users.xml

# Start Tomcat
catalina.sh run