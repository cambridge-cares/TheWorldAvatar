#########################
#
# This docker file creates an image for the Linked Fragement Server for Marie 
# 
# 
#
#
#########################



#########################
FROM tomcat:9.0
#########################
RUN apt-get update -y 
RUN apt-get install expect -y
RUN apt-get install openssh-client -y
 
 
#########################
# prepare the files (hdt, node modules)
#########################
COPY ./LDF_SERVER_JAVA/Server .
RUN mkdir /usr/src/app 
COPY ./docker/credentials /usr/src/app
RUN /usr/bin/expect /usr/src/app/download-hdt.sh
RUN rm /usr/src/app/host.txt 
RUN rm /usr/src/app/password.txt 
RUN cp ./kg/* ./
RUN cp ./ontokin_clean.hdt /usr/src/app/ontokin_clean.hdt
RUN apt-get install unzip -y
RUN unzip node_modules.zip -d /usr/src/app
#########################


#########################
# set up the LDF server and the redis cache server  
#########################
RUN apt install maven -y 
RUN mvn -Dmaven.javadoc.skip=true install
RUN mv ./target/ldfserver-0.2.4.war /usr/local/tomcat/webapps/ldfserver.war
RUN apt-get install redis-server -y
#########################


######################### 
# install nodejs server 
######################### 
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash
RUN apt-get install nodejs -y --fix-missing
WORKDIR /usr/src/app
COPY ./CACHE_SERVER .
RUN npm install
RUN npm install pm2 -g
######################### 

 

ENTRYPOINT ["/bin/bash","-c", "redis-server --daemonize yes && /usr/local/tomcat/bin/startup.sh && pm2-runtime comunica_node.js && bash"]
