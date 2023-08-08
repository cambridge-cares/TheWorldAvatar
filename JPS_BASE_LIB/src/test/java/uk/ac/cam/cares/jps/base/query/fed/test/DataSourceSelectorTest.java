package uk.ac.cam.cares.jps.base.query.fed.test;

import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.fed.DataSourceSelector;
import uk.ac.cam.cares.jps.base.query.fed.ParsedQueryTreeVisitor.ServiceGraphPatternSummary;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer;

public class DataSourceSelectorTest extends QueryProvider {
	
	static final Logger LOGGER = LogManager.getLogger(DataSourceSelectorTest.class);
	
	private ServiceDescriptionIndexer getSmallTestIndexer() {
		return ServiceDescriptionIndexerTest.getSmallTestIndexer();
	}

	private void assertKeySuffixes(List<String> keys, int expectedNumber, String[] expectedSuffixes) {
		assertEquals(expectedNumber, keys.size());
		int count = 0;
		for (String suffix : expectedSuffixes) {
			for (String key : keys) {
				if (key.endsWith(suffix)) {
					count++;
					break;
				}
			}
		}
		assertEquals(expectedNumber, count);
	}
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		setServiceUrlParams("http://my.local.host:8080/myblazegraph", false);
	}
	
	public void testExtractKeysForSubqueries() {
		setQueryFormatParams(false, true, false);
		Query query = getSparqlDistributedLab_1();
		String sparql = query.sparql;
		List<ServiceGraphPatternSummary> summaries = DataSourceSelector.extractKeysForSubqueries(sparql);
		
		assertEquals(2, summaries.size());
		assertKeySuffixes(summaries.get(0).keys, 4, new String[] {"hasYield", "hasValue", "hasNumericalValue", "hasUnit" });
		assertKeySuffixes(summaries.get(1).keys, 2, new String[] {"utilisesHistoricalData", "refersTo" });	
	}
	
	public void testAddValuesClause_1() {
		setQueryFormatParams(false, true, false);
		Query query = getSparqlDistributedLab_1();
		String sparql = query.sparql;
		sparql += "ORDER BY ASC(?exp)\r\n" + "LIMIT 1\r\n";
		
		String varName = "service1";
		String[] varValues = new String[] {
				"http://localhost:8080/blazegraph/namespace/lab_1/sparql",
				"http://localhost:8080/blazegraph/namespace/lab_2/sparql"
		};
		sparql = DataSourceSelector.addValuesClause(sparql, varName, Arrays.asList(varValues));
		varName = "service2";
		varValues = new String[] {"http://localhost:8080/blazegraph/namespace/doe_chemrxn/sparql"};
		sparql = DataSourceSelector.addValuesClause(sparql, varName, Arrays.asList(varValues));
		
		assertTrue(sparql.contains("VALUES ?service1 { <http://localhost:8080/blazegraph/namespace/lab_1/sparql> "
				+ "<http://localhost:8080/blazegraph/namespace/lab_2/sparql> }"));
		assertTrue(sparql.contains("VALUES ?service2 { <http://localhost:8080/blazegraph/namespace/doe_chemrxn/sparql> }"));
	}
	
	public void testAddValuesClause_2() {
		setQueryFormatParams(false, true, false);
		Query query = getSparqlDistributedLab_2();
		String sparql = query.sparql;
		
		String varName = "service1";
		String[] varValues = new String[] {
				"http://localhost:8080/blazegraph/namespace/lab_1/sparql",
				"http://localhost:8080/blazegraph/namespace/lab_2/sparql"
		};
		sparql = DataSourceSelector.addValuesClause(sparql, varName, Arrays.asList(varValues));
		
		assertTrue(sparql.contains("VALUES ?service1 { <http://localhost:8080/blazegraph/namespace/lab_1/sparql> "
				+ "<http://localhost:8080/blazegraph/namespace/lab_2/sparql> }"));
	}
	
	public void testAddValuesClause_3() {
		setQueryFormatParams(false, true, false);
		Query query = getSparqlBiodieselCityGML();
		String sparql = query.sparql;
		
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		DataSourceSelector selector = new DataSourceSelector(indexer);
		sparql = selector.addValuesClauses(sparql);
		
		assertTrue(sparql.contains("VALUES ?service1 { <http://www.theworldavatar.com/blazegraph/namespace/sgbiodieselplants/sparql> }"));
		assertTrue(sparql.contains("VALUES ?service2 { <http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql> }"));
	}
	
	public void testAddValuesClausesWithGivenServiceIRIs() throws Exception {
		setQueryFormatParams(false, true, false);
		Query query = getSparqlLocalWikidataWithPubChemCID887();
		String sparql = query.sparql;
		
		sparql = sparql.replace("?serva", "<http://www.theworldavatar.com/blazegraph/namespace/ontospecies/sparql>");
		sparql = sparql.replace("?servb", "<https://query.wikidata.org/sparql>");
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		DataSourceSelector selector = new DataSourceSelector(indexer);
		String newSparql = selector.addValuesClauses(sparql);
		
		// assert that no VALUES clauses have been added
		assertFalse(newSparql.contains("VALUES"));
		assertEquals(sparql, newSparql);
	}
	
	public void testAddValuesClausesWithGivenValueClauses() {
		setQueryFormatParams(false, true, true);
		Query query = getSparqlLocalWikidataWithPubChemCID887();
		String sparql = query.sparql;
		
		ServiceDescriptionIndexer indexer = getSmallTestIndexer();
		DataSourceSelector selector = new DataSourceSelector(indexer);
		String newSparql = selector.addValuesClauses(sparql);
		
		// assert that no additional VALUES clauses have been added
		assertEquals(sparql, newSparql);
	}
}
