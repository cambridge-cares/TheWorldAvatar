package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryFactory;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryInterface;

// https://rdf4j.org/documentation/programming/federation/
/**
 * One of multiple test configurations, see {@link QueryProvider} for details.
 */
public class FedXIntegrationTest extends QueryProvider { 
	
	private static final Logger LOGGER = LogManager.getLogger(FedXIntegrationTest.class);
	
	// @Override
	// public void setUp() throws Exception {
	// 	super.setUp();
	// 	TripleStoreProvider.getInstance();
	// 	setQueryFormatParams(false, false, false);
	// }
	
	// protected void queryAndAssert(Query query) {
	// 	queryAndAssert(query.sparql, query.getEndpoints(), query.result);
	// }
		
	// private void queryAndAssert(String sparql, List<String> endpoints, String expectedResult) {
	// 	endpoints = getIndexer().getEndpointUrls();
	// 	LOGGER.debug("FedX with number of endpoints=" + endpoints.size());
	// 	FederatedQueryInterface repo = FederatedQueryFactory.createFedX(endpoints);
	// 	String actualJson = repo.executeFederatedQuery(sparql);
	// 	assertQueryResult(expectedResult, actualJson);
	// }

	// public void testSparqlDistributedLab_1() {
	// 	queryAndAssert(getSparqlDistributedLab_1());
	// }
	
	// public void testSparqlDistributedLab_2() {
	// 	queryAndAssert(getSparqlDistributedLab_2());
	// }
	
	// public void testSparqlDistributedLab_3() {
	// 	queryAndAssert(getSparqlDistributedLab_3());
	// }
	
	// public void testSparqlDistributedLab_3_inverted_service_order() {
	// 	queryAndAssert(getSparqlDistributedLab_3_inverted_service_order());
	// }
	
	// public void testSparqlOntoSpeciesOntoCompChemSmall() {
	// 	queryAndAssert(getSparqlOntoSpeciesOntoCompChemSmall());
	// }
	
	// public void testSparqlOntoSpeciesOntoCompChemMedium() {
	// 	queryAndAssert(getSparqlOntoSpeciesOntoCompChemMedium());
	// }
	
	// public void testSparqlOntoSpeciesOntoCompChemLarge() {
	// 	queryAndAssert(getSparqlOntoSpeciesOntoCompChemLarge());
	// }
	
	// public void xxxtestRemoteSparqlBiodieselCityGML() {
	// 	queryAndAssert(getSparqlBiodieselCityGML());
	// }
	
	// public void xxxtestRemoteSparqlWikidataDBpedia() {
	// 	queryAndAssert(getSparqlWikidataDBpedia());
	// }
}
