package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryFactory;
import uk.ac.cam.cares.jps.base.query.fed.FederatedQueryInterface;

// https://rdf4j.org/documentation/programming/federation/
public class FedXIntegrationTest extends QueryProvider { 
	
	private static final Logger LOGGER = LogManager.getLogger(FedXIntegrationTest.class);
	
	boolean useAllEndpoints = false; 
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		TripleStoreProvider.getInstance();
		setQueryFormatParams(false, false, false);
		useAllEndpoints = false; 
	}
	
	protected void queryAndAssert(Query query) {
		queryAndAssert(query.sparql, query.getEndpoints(), query.result);
	}
		
	private void queryAndAssert(String sparql, List<String> endpoints, String expectedResult) {
		LOGGER.debug("FedX with number of endpoints=" + endpoints.size());
		if (useAllEndpoints) {
			endpoints = QueryProvider.getIndexer(true).getEndpointUrls();
		}		
		FederatedQueryInterface repo = FederatedQueryFactory.createFedX(endpoints);
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
	
	public void xxxtestRemoteSparqlBiodieselCityGML() {
		queryAndAssert(getSparqlBiodieselCityGML());
	}
	
	public void xxxtestRemoteSparqlWikidataDBpedia() {
		queryAndAssert(getSparqlWikidataDBpedia());
	}
	
	public void xxxtestRemoteSparqlOntoSpeciesOntoCompChemSmall() {
		queryAndAssert(getSparqlOntoSpeciesOntoCompChemSmall());
	}
	
	public void xxxtestRemoteSparqlLocalWikidataWithPubChemCID887() {
		queryAndAssert(getSparqlLocalWikidataWithPubChemCID887());
	}
	

}
