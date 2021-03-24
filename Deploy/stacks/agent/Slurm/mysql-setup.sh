#!/usr/bin/expect

#spawn mysql_secure_installation

# Set the root password
set password [printenv MYSQL_ROOT_PASS]
send "echo Hello World"
send "echo $password"

#expect "*password for root*"
#send "$password\r";