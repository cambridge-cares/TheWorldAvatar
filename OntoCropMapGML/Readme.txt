#########################
# Blazegraph deployment #
#########################
Blazegraph can be deployed in the following two ways. You can choose one depending upon your requirement.

a) Deploy the blazegraph.jar file using the property file RWStore.properties using the following command:

   java -server -Xmx4g -Dbigdata.propertyFile=RWStore.properties -jar blazegraph.jar

b) Deploy the blazegraph.war file on a Tomcat server.

#########################
# Namespace creation    #
#########################
Namespaces on Blazegraph are the same as repositories on RDF4J. While creating the "landuse" namespace
for uploading Crop Map GML data to Blazegraph, select the "Enable Geospatial" option, and click on the "Create namespace"
button. In the pop-up window, replace all configuration parameters with parameters available in the RWStore.properties file.
Finally, click on the "Create" button.

##########################
# Vocabulary creation    #
##########################
If you want to add a vocabulary to Blazegraph by cloning from the URL https://github.com/blazegraph/database.git
and build a war or jar file on your own, put PolygonVocabulary.java under the path of "database\bigdata-core\bigdata-rdf\src\java\com\bigdata\rdf\vocab"

In the next step, build the following maven projects:
1. database\pom.xml
2. database\blazegraph-war\pom.xml (for creating a war file)
3. database\blazegraph-jar\pom.xml (for creating a jar file)
