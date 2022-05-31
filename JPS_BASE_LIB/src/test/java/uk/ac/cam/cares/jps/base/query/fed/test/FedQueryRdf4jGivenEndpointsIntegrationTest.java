package uk.ac.cam.cares.jps.base.query.fed.test;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryFactory;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryInterface;

/**
 * One of multiple test configurations, see {@link QueryProvider} for details.
 */
public class FedQueryRdf4jGivenEndpointsIntegrationTest extends QueryProvider {
	
	private static final Logger LOGGER = LogManager.getLogger(FedQueryRdf4jGivenEndpointsIntegrationTest.class);
	
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
		LOGGER.debug("Federated query for RDF4J with given service endpoints and without data selection");
		String fedEngineUrl = TripleStoreProvider.getEndpointUrl(TripleStoreProvider.NAMESPACE_RDF4J_EMPTY);
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
	
	public void testRemoteSparqlOntoSpeciesOntoCompChemSmall() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemSmall());
	}
	
	public void testRemoteSparqlOntoSpeciesOntoCompChemMedium() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemMedium());
	}
	
	public void xxxtestRemoteSparqlOntoSpeciesOntoCompChemLarge() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemLarge());
	}
	
	public void xxxtestRemoteSparqlBiodieselCityGML() {
		queryAndAssert(getSparqlBiodieselCityGML());
	}
	
	public void xxxtestRemoteSparqlWikidataDBpedia() {
		queryAndAssert(getSparqlWikidataDBpedia());
	}
}
