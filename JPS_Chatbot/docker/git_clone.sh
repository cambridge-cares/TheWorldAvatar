#!/usr/bin/expect -f

set timeout -1

spawn git clone ssh://zhou0201@vienna.cheng.cam.ac.uk/home/userspace/CoMoCommon/Codes/CARES/JParkSimulator-git

expect "*yes*"

send "yes\n"

expect "*password*" 

send "tvDm=Zt24\n"

expect eof
