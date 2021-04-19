#!/usr/bin/expect -f

#
# Downloads HDT and Node libraries.
# Author: Xiaochi Zhou (xz378<@>cam.ac.uk)
#

set timeout -1

# Proceedure to read contents of a file
proc slurp {file} {
    set fh [open $file r]
    set ret [read -nonewline $fh]
    close $fh
    return $ret
}

# Read the host file
puts "Reading host.txt file..."
set host [slurp /usr/src/app/host.txt]
puts "Read host.txt file"

# Read the password file
puts "Reading password.txt file..."
set password [slurp /usr/src/app/password.txt]
puts "Read password.txt file"

# Download "hdt and node libraries" files
puts "Downloading from 'kg' directory..."
spawn scp -r $host:/home/userspace/CoMoCommon/Archive/Projects/Preprints/c4e/c4e-266-xz378-Chatbot/Data/kg .

expect "*authenticity*"
send "yes\r" 
expect "*assword*" 
send "$password\r"
expect eof
puts "Download complete."

# Finish
puts "All downloads complete, consider removing host and password files."