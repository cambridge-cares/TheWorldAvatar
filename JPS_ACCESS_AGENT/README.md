## Integration Tests

The integration tests are designed to test the 
AccessAgentCaller class together with the AccessAgent,
StoreRouter and the RemoteStoreClient and FileBasedStoreClient.

Requirements to run the AccessAgentRemoteStoreIntegrationTest:
1. 	AccessAgent deployed on "http://localhost:8080"
2. 	Blazegraph deployed on "http://localhost:8080", 
	with the namespace (triple store) "teststorelocal" setup 
	and accessible at the endpoint "http://localhost:8080/blazegraph/namespace/teststorelocal/sparql"
3.	Internet connection to the OntoKGRouter triple store (currently at "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql")
4. 	OntoKGROuter is expected to contain routing to endpoint "http://localhost:8080/blazegraph/namespace/teststorelocal/sparql" for the resource "http://kb/teststorelocal"

Requirements to run the AccessAgentFileBasedStoreIntegrationTest:
1. 	AccessAgent deployed on "http://localhost:8080"
2.	The tomcat root path is assumed to be "C:/TOMCAT/webapps/ROOT" (consistent with Claudius and OntoKGROuter routing information)
3.	Internet connection to the OntoKGRouter triple store (currently at "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql")
4. 	OntoKGRouter is assumed to contain "tomcatrootpath" with the value "file:///C:/TOMCAT/webapps/ROOT", for filebased routing (see #2.)