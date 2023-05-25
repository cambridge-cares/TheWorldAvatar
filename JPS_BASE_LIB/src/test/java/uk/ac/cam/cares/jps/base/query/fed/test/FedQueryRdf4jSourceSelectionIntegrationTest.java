package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryFactory;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryInterface;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer;

/**
 * One of multiple test configurations, see {@link QueryProvider} for details.
 */
public class FedQueryRdf4jSourceSelectionIntegrationTest extends QueryProvider {
	
	private static final Logger LOGGER = LogManager.getLogger(FedQueryRdf4jSourceSelectionIntegrationTest.class);
	private static FederatedQueryInterface engine = null;
	private static List<String> endpoints = null;
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		TripleStoreProvider.getInstance();
		setQueryFormatParams(false, true, false);
		setServiceUrlParams(null, true);
		if (engine == null) {
			LOGGER.info("Creating engine with endpoint selection");
			String fedEngineUrl = TripleStoreProvider.getEndpointUrl(TripleStoreProvider.NAMESPACE_RDF4J_EMPTY);			
			ServiceDescriptionIndexer indexer = getIndexer();
			endpoints = indexer.getEndpointUrls();
			Map<String,String> host2host = TripleStoreProvider.getHostConversionMap();
			engine = FederatedQueryFactory.createWithEndpointSelection(fedEngineUrl, indexer, host2host);
		}
	}
	
	protected void queryAndAssert(Query query) {
		queryAndAssert(query.sparql, query.result);
	}
	
	private void queryAndAssert(String sparql, String expectedResult) {
		LOGGER.debug("Federated query for RDF4J with endpoint selection");
		LOGGER.debug("number of endpoints=" + endpoints.size());
		String actualJson = engine.executeFederatedQuery(sparql);	
		assertQueryResult(expectedResult, actualJson);
	}
	
	// https://rdf4j.org/documentation/reference/rest-api/#repository-list
	private List<String> getRDF4JEndpoints(String serviceUrl) {
		List<String> endpoints = new ArrayList<String>();
		
		String url = serviceUrl + "/repositories";
		String accept = "application/sparql-results+json";
		HttpGet request = Http.get(url, accept);
		String result = Http.execute(request);
		JSONArray ja = JenaResultSetFormatter.convertToSimplifiedJsonArray(result);
		for (int i=0; i<ja.length(); i++) {
			JSONObject jo = ja.getJSONObject(i);
			String endpoint = jo.getString("uri");
			endpoints.add(endpoint);
		}
		
		return endpoints;
	}
	
	public void testNamespaceEmptyWasCreated() {
		String serviceUrl = TripleStoreProvider.getServiceUrl(TripleStoreProvider.ID_RDF4J_1);
		String namespace = TripleStoreProvider.NAMESPACE_RDF4J_EMPTY;
		List<String>  endpoints = getRDF4JEndpoints(serviceUrl);
		LOGGER.debug("endpoints=" + endpoints);
		boolean found = false;
		for (String endpoint : endpoints) {
			if (endpoint.endsWith(namespace)) {
				found = true;
				break;
			}
		}
		assertTrue("endpoint was not found", found);
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
	
	public void xxxtestSparqlOntoSpeciesOntoCompChemLarge() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemLarge());
	}
	
	public void xxxtestRemoteSparqlBiodieselCityGML() {
		queryAndAssert(getSparqlBiodieselCityGML());
	}
	
	public void xxxtestRemoteSparqlWikidataDBpedia() {
		queryAndAssert(getSparqlWikidataDBpedia());
	}
}
