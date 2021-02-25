#!/usr/bin/expect -f

set timeout -1

spawn sudo git clone ssh://zhou0201@vienna.cheng.cam.ac.uk/home/userspace/CoMoCommon/Codes/CARES/JParkSimulator-git

  

expect eof
