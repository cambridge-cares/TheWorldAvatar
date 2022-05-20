#!/bin/sh

# If the password file exists, setup the authorisation

if [ -f /usr/local/password-file.txt ]; then
	echo "Password file detected, will setup HTTP authorisation for credo-user..."
	
    password=`cat /usr/local/password-file.txt`
	mkdir /etc/apache2
	htpasswd -c -b /etc/apache2/.htpasswd credo-user "$password"
	
	echo "Credentials for credo-user account have been created."
else
	echo "No password file found, will skip HTTP authorisation for credo-user."
fi


