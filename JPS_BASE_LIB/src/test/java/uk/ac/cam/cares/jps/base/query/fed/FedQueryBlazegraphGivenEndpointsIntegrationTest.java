package uk.ac.cam.cares.jps.base.query.fed;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Ignore;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

@Ignore("The code this tests is not used and takes a long time to run.")
/**
 * One of multiple test configurations, see {@link QueryProvider} for details.
 */
public class FedQueryBlazegraphGivenEndpointsIntegrationTest extends QueryProvider {
	
	private static final Logger LOGGER = LogManager.getLogger(FedQueryBlazegraphGivenEndpointsIntegrationTest.class);
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		TripleStoreProvider.getInstance();
		setQueryFormatParams(false, true, true);
		setServiceUrlParams(null, true);
	}
	
	protected void queryAndAssert(Query query) {
		queryAndAssert(query.sparql, query.result);
	}
	
	private void queryAndAssert(String sparql, String expectedResult) {
		LOGGER.debug("Federated query for Blazegraph with given service endpoints and without data selection");
		String fedEngineUrl = TripleStoreProvider.getEndpointUrl(TripleStoreProvider.NAMESPACE_BLAZEGRAPH_EMTPY);
		FederatedQueryInterface repo = FederatedQueryFactory.createForQueriesWithGivenEndpoints(fedEngineUrl);
		String actualJson = repo.executeFederatedQuery(sparql);	
		assertQueryResult(expectedResult, actualJson);
	}
	
	public void testSparqlDistributedLab_1() {
		queryAndAssert(getSparqlDistributedLab_1());
	}
	
	public void testSparqlDistributedLab_2() {
		queryAndAssert(getSparqlDistributedLab_2());
	}
	
	public void testSparqlDistributedLab_3() {
		queryAndAssert(getSparqlDistributedLab_3());
	}
	
	public void testSparqlDistributedLab_3_inverted_service_order() {
		queryAndAssert(getSparqlDistributedLab_3_inverted_service_order());
	}
	
	public void testSparqlOntoSpeciesOntoCompChemSmall() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemSmall());
	}
	
	public void testSparqlOntoSpeciesOntoCompChemMedium() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemMedium());
	}
	
	public void testSparqlOntoSpeciesOntoCompChemLarge() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemLarge());
	}
	
	public void xxxtestRemoteSparqlBiodieselCityGML() {
		queryAndAssert(getSparqlBiodieselCityGML());
	}
	
	public void xxxtestRemoteSparqlWikidataDBpedia() {
		queryAndAssert(getSparqlWikidataDBpedia());
	}
	
	private String createEndpoint(String containerId, String namespace) {
		TripleStoreProvider provider = TripleStoreProvider.getInstance();
		String path = BlazegraphRepositoryWrapper.getPathForBlazegraph(namespace);
		provider.createDatasetBlazegraph(containerId, namespace, path, null);
		return TripleStoreProvider.getEndpointUrl(namespace);
	}
	
	public void testSelectDistinctSolvesProblemWithDuplicatedRows() throws IOException {
		
		String prefixes = "PREFIX twa: <http://www.theworldavatar.com/test#>\r\n"
				+ "PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\r\n"
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontospecies:casRegistryID#>\r\n";
		
		// create first endpoint and insert triples
		String namespace1 = "duplicate_1";
		String url1 = createEndpoint(TripleStoreProvider.ID_BLAZEGRAPH_1, namespace1);
		String sparql = prefixes
				+ "INSERT DATA {\r\n"
				+ "  twa:1001 ontospecies:casRegistryID 1001 .\r\n"
				+ "  twa:1002 ontospecies:casRegistryID 1002 .\r\n"
				+ "  twa:1003 ontospecies:casRegistryID 1003 .\r\n"
				+ "  twa:1004 ontospecies:casRegistryID 1004 .\r\n"
				+ "}";
		RemoteStoreClient client = new RemoteStoreClient(url1, url1);
		client.executeUpdate(sparql);
		
		//sparql = prefixes + "SELECT * WHERE { ?s ?p ?o }";
		//JSONArray result = client.executeQuery(sparql);
		//System.out.println("RESULT 1 = \n" + result);
		
		// create second endpoint and insert triples
		String namespace2 = "duplicate_2";
		String url2 = createEndpoint(TripleStoreProvider.ID_BLAZEGRAPH_2, namespace2);
		sparql = prefixes
				+ "INSERT DATA {\r\n"
				+ "  twa:1001_1 ontocompchem:hasUniqueSpecies twa:1001 .\r\n" 		// + 1
				+ "  twa:1001_2 ontocompchem:hasUniqueSpecies twa:1001 .\r\n" 		// + 3
				+ "  twa:1001_3 ontocompchem:hasUniqueSpecies twa:1001 .\r\n" 		// + 5
				//+ "  twa:1001_4 ontocompchem:hasUniqueSpecies twa:1001 .\r\n"		// + 7
				+ "  twa:1002_1 ontocompchem:hasUniqueSpecies twa:1002 .\r\n"		// + 1
				+ "  twa:1002_2 ontocompchem:hasUniqueSpecies twa:1002 .\r\n"		// + 3
				+ "  twa:1003_1 ontocompchem:hasUniqueSpecies twa:1003 .\r\n"		// + 1 (1003_1 with existing species in duplicate_1)
				+ "  twa:1003_1 ontocompchem:hasUniqueSpecies twa:1043 .\r\n"		// + 0 (1003_1 with non-existing species in duplicate_1)
				+ "  twa:1043_1 ontocompchem:hasUniqueSpecies twa:1043 .\r\n"		// + 0 (another IRI with non-existing species in duplicate_1)
				+ "}";
		client = new RemoteStoreClient(url2, url2);
		client.executeUpdate(sparql);
		
		// execute the federated query
		String fedQuery = prefixes 
				+ "SELECT  *\r\n"
				+ "WHERE {\r\n"
				+ "  SERVICE <%s> {\r\n"
				+ "    ?compchemspecies ontocompchem:hasUniqueSpecies ?species .\r\n"
				+ "  }\r\n"
				+ "  SERVICE <%s> {\r\n"
				+ "    ?species ontospecies:casRegistryID ?crid .\r\n"
				+ "  }\r\n"
				+ "}";
		String dockerUrl1 = TripleStoreProvider.getDockerEndpointUrl(namespace1);
		String dockerUrl2 = TripleStoreProvider.getDockerEndpointUrl(namespace2);
		fedQuery = String.format(fedQuery, dockerUrl2, dockerUrl1);
		
		// 
		String expected = "6";
		
		// check that there 14 rows instead
		String actual = "14";  		// = 9 + 4 + 1
		queryAndAssert(fedQuery, actual);
		
		// use DISTINCT to eliminate the duplicated rows
		fedQuery = fedQuery.replace("SELECT", "SELECT DISTINCT");
		queryAndAssert(fedQuery, expected);
	}
}
