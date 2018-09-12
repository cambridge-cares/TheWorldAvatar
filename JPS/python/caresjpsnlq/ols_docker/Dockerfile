FROM ubuntu:16.04

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
RUN echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | tee /etc/apt/sources.list.d/mongodb.list
RUN apt-get update && apt-get install -y \
		openjdk-8-jdk \
		maven \
		tomcat7 \
		mongodb-org \
		software-properties-common \
		wget \
		git \
		nano

ENV OLS_HOME /opt/OLS
ENV CATALINA_OPTS "-Xms2g -Xmx2g"
## The 3.1.0.RELEASE has a bug with a hard coded path therefor we use  this commit
ENV OLS_VERSION dc03b5fc6bd46b4052e231dea046d1d4bba6037d
ENV SOLR_VERSION 5.5.3

ADD ols-config.yaml /tmp/		
ADD obo-config.yaml /tmp/	
## The install_solr_service.sh version from solr 5.5.3 has an issue with certain docker configurations
## Therefor we use the script of solr 6.3.0.  solr 6.3.0 is not compatible with OLS 
ADD 630_install_solr_service.sh /tmp/install_solr_service.sh

## Prepare MongoDB directories
RUN mkdir /data/ 
RUN mkdir /data/db 

## Clone GIT repository 
RUN git clone https://github.com/EBISPOT/OLS.git /opt/OLS \
  && cd  /opt/OLS \
  && git checkout ${OLS_VERSION}

### Install and stop solr 
RUN cd /opt \
  && wget http://archive.apache.org/dist/lucene/solr/${SOLR_VERSION}/solr-${SOLR_VERSION}.tgz \
	#&& tar xzf solr-${SOLR_VERSION}.tgz solr-${SOLR_VERSION}/bin/install_solr_service.sh --strip-components=2 \
	&& cp /tmp/install_solr_service.sh /opt/install_solr_service.sh \
  && bash ./install_solr_service.sh solr-${SOLR_VERSION}.tgz \
	&& service solr stop 

## Prepare configuration files
RUN sed -i '$a ols.home /opt/OLS' /opt/OLS/ols-web/src/main/resources/application.properties 
RUN mv /opt/OLS/ols-apps/ols-config-importer/src/main/resources/ols-config.yaml /opt/OLS/ols-apps/ols-config-importer/src/main/resources/ols-config.yamlorig 
RUN mv /opt/OLS/ols-apps/ols-config-importer/src/main/resources/obo-config.yaml /opt/OLS/ols-apps/ols-config-importer/src/main/resources/obo-config.yamlorig 
RUN cp /tmp/ols-config.yaml /opt/OLS/ols-apps/ols-config-importer/src/main/resources/ 
RUN cp /tmp/obo-config.yaml /opt/OLS/ols-apps/ols-config-importer/src/main/resources/ 
RUN sed -i '6 s/^/#/' /opt/OLS/ols-apps/ols-config-importer/src/main/resources/application.properties 

##Maven build 
RUN cd /opt/OLS/ \
	&& mvn clean install -Dols.home=/opt/OLS 

## Start MongoDB and 
### Load configuration into MongoDB
RUN mongod --fork --logpath /var/log/mongodb.log \
	&& java -Dols.home=/opt/OLS -jar /opt/OLS/ols-apps/ols-config-importer/target/ols-config-importer.jar \
	&& sleep 10

## Start MongoDB and SOLR
## Then start the indexation process cd /etc/init.d/
ENV MEMORY_USE "-Xmx4g -Xms4g"
RUN mongod --fork --logpath /var/log/mongodb.log \
  && /opt/solr/bin/solr -Dsolr.solr.home=/opt/OLS/ols-solr/src/main/solr-5-config/ -Dsolr.data.dir=/opt/OLS \  
	&& java ${MEMORY_USE} -Dols.home=/opt/OLS -jar /opt/OLS/ols-apps/ols-loading-app/target/ols-indexer.jar  

## Copy webapp to tomcat dir, replace the ROOT webapplication with boot-ols.war and set permissions
RUN rm -R /var/lib/tomcat7/webapps/ROOT/
RUN cp /opt/OLS/ols-web/target/ols-boot.war /var/lib/tomcat7/webapps/ROOT.war
RUN chown -R tomcat7:tomcat7 /opt/OLS/

## Expose the tomcat port 
EXPOSE 8080

## Add the bootstrap file and make it executable
ADD bootstrap.sh /opt/OLS/
RUN chmod +x /opt/OLS/bootstrap.sh

CMD /opt/OLS/bootstrap.sh
