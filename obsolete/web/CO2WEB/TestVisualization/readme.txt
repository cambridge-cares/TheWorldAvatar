*Files end with test are test Files.
*Files start with try_ are makeshift-example files of library usage.



*change_data_test
 * Test the registerer module: register to data-changing node
 * Test the /change route for data-changing node to post changed data
 * When /change route successfully connected, an update event should be emitted to socket clients
 * Test if socket client can receive this event and along with it, the changed data
 

*fileConnection_test
 * Test fileConnection module, which extracts all connections out of current grOWLs, starting from top node on disk(All subsequent nodes are accessed via internet)

*rdfParser_test
 *Testing the rdfParser, which extracts o by providing s and p in a s,p,o triple defined in owl file

*routes_test
 * test all http request routes defined by server can be accessed


	