#########################
#
# This docker file creates an image for the Linked Fragement Server for Marie 
# 
# 
#
#
#########################



# Set up the nodejs buffer server 
#########################
FROM dordoka/tomcat:tomcat8.5
#########################
COPY ./LDF_SERVER_JAVA/Server .
RUN mkdir /usr/src/app 
COPY ./ontokin_clean.hdt /usr/src/app/ontokin_clean.hdt
 
#########################


#########################
# COPY ldfserver.war ./webapps
RUN apt-get update -y 
RUN apt install maven -y
# RUN apt-get install software-properties-common -y 
# RUN apt-get update -y 
RUN add-apt-repository ppa:openjdk-r/ppa -y
RUN apt-get update -y 
RUN apt-get install openjdk-15-jdk --fix-broken -y

# update java version to 15 

RUN apt-get install redis-server -y
ENV JAVA_HOME /usr/lib/jvm/java-15-openjdk-amd64
ENV PATH $PATH:$JAVA_HOME/bin
RUN update-alternatives --set java /usr/lib/jvm/java-15-openjdk-amd64/bin/java
RUN update-alternatives --set javac /usr/lib/jvm/java-15-openjdk-amd64/bin/javac
#########################

# RUN apt-get install openjdk-11-jdk -y
RUN mvn -Dmaven.javadoc.skip=true install
RUN mv /opt/tomcat/target/ldfserver-0.2.4.war /opt/tomcat/webapps/ldfserver.war
# RUN ./bin/startup.sh
#########################

 
# install nodejs and run the cache server  
######################### 
# update 
# install curl 
RUN apt-get install curl -y 
# get install script and pass it to execute: 
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash
# and install node 
RUN apt-get install nodejs -y --fix-missing
# confirm that it was successful 
RUN node -v
# npm installs automatically 
RUN npm -v
RUN apt update -y 
RUN apt install vim -y 
######################### 
WORKDIR /usr/src/app
COPY ./CACHE_SERVER .
RUN npm install
RUN npm install pm2 -g
# CMD ["pm2-runtime", "comunica_node.js"]
#########################



# ENTRYPOINT ["supervisord", "--nodaemon", "--configuration", "/etc/supervisord.conf"]

ENTRYPOINT ["/bin/bash","-c", "redis-server --daemonize yes && /opt/tomcat/bin/startup.sh && pm2-runtime comunica_node.js  && bash"]
