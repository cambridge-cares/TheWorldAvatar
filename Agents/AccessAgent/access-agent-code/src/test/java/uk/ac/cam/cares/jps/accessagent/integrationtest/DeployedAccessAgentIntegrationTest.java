package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import java.net.URISyntaxException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.accessagent.AccessAgent;
import uk.ac.cam.cares.jps.accessagent.RDBAccessAgent;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RDBAccessAgentCaller;

/**
 * This tests the Access Agent DEPLOYED on TWA server. 
 * 
 * These tests are read-only, so SPARQL update is not tested.
 *   
 * @author csl37
 *
 */
@Disabled("Requires the AccessAgent to be deployed on a TWA server.")
class DeployedAccessAgentIntegrationTest {

	String host = KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST);
	String pathFirstPart = "/access-agent";
	String scheme = "http";
	
	@Test
	void testGetEndpoints() {

		String targetTripleStore = "ontokin";
		String expectedResult = "http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql";
		
		JSONObject result = AccessAgentCaller.getEndpoints(targetTripleStore);
		
		assertNotNull(result);
		
		String queryEndpoint = result.getString(JPSConstants.QUERY_ENDPOINT);
		String updateEndpoint = result.getString(JPSConstants.UPDATE_ENDPOINT);
		
		assertEquals(expectedResult,queryEndpoint);
		assertEquals(expectedResult,updateEndpoint);
	}
	
	@Test
	void testSparqlQuery() {

		String targetTripleStore = "ontokin";
		String sparqlQuery = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokin/ABF.owl> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o} LIMIT 1";
		String expectedResult = "http://www.w3.org/2002/07/owl#Ontology";
		
		JSONArray ja = AccessAgentCaller.queryStore(targetTripleStore, sparqlQuery);
		assertNotNull(ja);
		
		JSONObject jo = ja.getJSONObject(0); 
		assertEquals(expectedResult,jo.get("o").toString());
	}
		
	@Test
	void testTripleStoreClearCache() throws URISyntaxException {
				
		String url = new URI(scheme, host, pathFirstPart+AccessAgent.CLEAR_CACHE_URL, null, null).toString();
		
		String res = Http.execute(Http.get(url,null));
		assertNotNull(res);
		
		String strRes = new JSONObject(res).getString(JPSConstants.RESULT_KEY);
		assertEquals("Cache cleared.",strRes);		
	}

	@Test
	void testGetRDBUrl() {
		
		String targetRDB = "weatherTimeSeries";
		String expectedResult = "jdbc:postgresql://localhost:5432/weatherTimeSeries";
		
		String url = RDBAccessAgentCaller.getRDBUrl(targetRDB);

        assertEquals(expectedResult,url);
	}
	
	@Test
	void testRDBClearCache() throws URISyntaxException {
				
		String url = new URI(scheme, host, pathFirstPart+RDBAccessAgent.CLEAR_CACHE_URL, null, null).toString();
		
		String res = Http.execute(Http.get(url,null));
		assertNotNull(res);
		
		String strRes = new JSONObject(res).getString(JPSConstants.RESULT_KEY);
		assertEquals("Cache cleared.",strRes);		
	}
}
