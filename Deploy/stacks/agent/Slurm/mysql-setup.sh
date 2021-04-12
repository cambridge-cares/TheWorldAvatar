#!/usr/bin/expect

set timeout 10

# Read the root password
set f [open "/usr/local/secrets/mysql_root_pass"]
set password [split [read $f] "\n"]
close $f

# MySQL setup
spawn mysql_secure_installation

# Default MariaBd password is blank
expect "Enter current password for root*"
send "\r";

# Change the root password
expect "Change the root password*"
send "Y\r";

expect "New password:"
send "$password\r";

expect "Re-enter new password:"
send "$password\r";

# Delete anonymous users
expect "*anonymous users*"
send "Y\r";

# Prevent remote login
expect "*login remotely*"
send "Y\r";

# Delete test database
expect "*test database*"
send "Y\r";

# Reload privilegesY
expect "Reload privilege*"
send "Y\r";

interact