#########################
# Blazegraph deployment #
#########################
Blazegraph can be deployed in the following two ways. You can choose one depending upon your requirement.

a) Deploy the blazegraph.jar file using the property file RWStore.properties using the following command:

   java -server -Xmx4g -Dbigdata.propertyFile=RWStore.properties -jar blazegraph.jar

b) Deploy the blazegraph.war file on a Tomcat server.

Currently, both the jar and war files are available at the following link: https://www.dropbox.com/sh/woa51kecoa78ol1/AACgQ6muvZCcVWsJ_NdrWDH2a?dl=0

#########################
# Namespace creation    #
#########################
Namespaces on Blazegraph are the same as repositories on RDF4J. While creating the "ontocropmapgml" namespace
for uploading Crop Map GML data to Blazegraph, select the "Enable Geospatial" option, and click on the "Create namespace"
button. In the pop-up window, replace all configuration parameters with parameters available in the RWStore.properties file.
Finally, click on the "Create" button.