package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryFactory;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryInterface;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer;

/**
 * One of multiple test configurations, see {@link QueryProvider} for details.
 */
public class FedQueryBlazegraphSourceSelectionIntegrationTest extends QueryProvider {
	
	private static final Logger LOGGER = LogManager.getLogger(FedQueryBlazegraphSourceSelectionIntegrationTest.class);
	
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
		LOGGER.debug("Federated query for Blazegraph with endpoint selection");
		String fedEngineUrl = TripleStoreProvider.getEndpointUrl(TripleStoreProvider.NAMESPACE_BLAZEGRAPH_EMTPY);
		ServiceDescriptionIndexer indexer = getIndexer();
		Map<String,String> host2host = TripleStoreProvider.getHostConversionMap();
		FederatedQueryInterface repo = FederatedQueryFactory.createWithEndpointSelection(fedEngineUrl, indexer, host2host);
		//FederatedQueryInterface repo = FederatedQueryFactory.createForQueriesWithGivenEndpoints(fedEngineUrl);
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
	
	public void testRemoteSparqlOntoSpeciesOntoCompChemSmall() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemSmall());
	}
	
	public void testRemoteSparqlOntoSpeciesOntoCompChemMedium() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemMedium());
	}
	
	public void testRemoteSparqlOntoSpeciesOntoCompChemLarge() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemLarge());
	}
	
	public void xxxtestRemoteSparqlBiodieselCityGML() {
		queryAndAssert(getSparqlBiodieselCityGML());
	}
	
	public void xxxtestRemoteSparqlWikidataDBpedia() {
		queryAndAssert(getSparqlWikidataDBpedia());
	}
}
