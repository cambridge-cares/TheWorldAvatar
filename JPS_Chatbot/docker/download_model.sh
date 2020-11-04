#!/usr/bin/expect -f

# download the model form vienna

set timeout -1

spawn scp -r zhou0201@vienna.cheng.cam.ac.uk:/home/userspace/CoMoCommon/Ongoing/Projects/c4e-xz378-Chatbot/models/models_wiki/models ./JParkSimulator-git/JPS_Chatbot/UI/source
 
expect "*password*" 

send "tvDm=Zt24\n"

expect eof

spawn scp -r zhou0201@vienna.cheng.cam.ac.uk:/home/userspace/CoMoCommon/Ongoing/Projects/c4e-xz378-Chatbot/models/models_jps/models ./JParkSimulator-git/JPS_Chatbot/UI/source/rasa_jps
 
expect "*password*" 

send "tvDm=Zt24\n"

expect eof