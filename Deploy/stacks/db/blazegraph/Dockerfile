FROM tomcat:9.0

# Download Blazegraph .war file
RUN wget -O /usr/local/tomcat/webapps/blazegraph.war https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_2_1_6_RC/blazegraph.war

# Install zip in order to modify .war file
RUN apt-get update && apt-get install zip -y && rm -rf /var/lib/apt/lists/*

# Overwrite tomcat-users.xml to add blazegraph role
COPY tomcat/conf/tomcat-users.xml /usr/local/tomcat/conf/tomcat-users.xml

# Configuration files can't just be copied in, as they'd get overwritten when the war file is
# deployed. Instead, copy them to a temporary location and use an entrypoint script to add them to
# the war file before tomcat is started.
COPY blazegraph_war /tmp/blazegraph

# Retrieve a newer version of the Apache Commons IO library to fix a bug with inserting JSON-LD files.
ADD https://repo1.maven.org/maven2/commons-io/commons-io/2.16.1/commons-io-2.16.1.jar /tmp/blazegraph/WEB-INF/lib/
# Delete the old version
RUN zip -d /usr/local/tomcat/webapps/blazegraph.war WEB-INF/lib/commons-io-2.1.jar

# Copy in script and set it as the entry point
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]