#!/usr/bin/expect -f

#
# Downloads trained model files required by Wikidata_Query before building into an Image.
# Author: Michael Hillman (mdhillman<@>cmclinnovations.com)
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
set host [slurp host.txt]
puts "Read host.txt file"

# Read the password file
puts "Reading password.txt file..."
set password [slurp password.txt]
puts "Read password.txt file"

# Download "models_wiki" files
# /home/userspace/CoMoCommon/Archive/Projects/Preprints/c4e/c4e-266-xz378-Chatbot/Data/models
puts "Downloading from 'models_wiki' directory..."
spawn scp -r $host:/home/userspace/CoMoCommon/Archive/Projects/Preprints/c4e/c4e-266-xz378-Chatbot/Data/models/models_wiki_0707/models /tmp/models_wiki

expect "*authenticity*"
send "yes\r" 
expect "*assword*" 
send "$password\r"
expect eof
puts "Download complete."

# Download "models_jps" files
puts "Downloading from 'models_jps' directory..."
spawn scp -r $host:/home/userspace/CoMoCommon/Archive/Projects/Preprints/c4e/c4e-266-xz378-Chatbot/Data/models/models_jps_0413/models /tmp/models_jps
 
expect "*assword*" 
send "$password\r"
expect eof
puts "Download complete."

# Finish
puts "All downloads complete, consider removing host and password files."