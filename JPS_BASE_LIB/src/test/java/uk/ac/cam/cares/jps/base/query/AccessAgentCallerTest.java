package uk.ac.cam.cares.jps.base.query;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.After;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

public class AccessAgentCallerTest {

	@BeforeAll
	static void setAccessAgentHostUrl(){
		//set default accessagent_host to value in jps.properties
		AccessAgentCaller.accessAgentHost = KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST);
	}

	@After
	public void tearDown() {
		JPSContext.removeJPSContext();
	}
	
	@Test
	public void testCreateRequestUrlCase1() throws UnsupportedEncodingException, URISyntaxException {
			
		String expectedPath = JPSConstants.ACCESS_AGENT_PATH;
		String expectedRequestUrl = "http://"+KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST)+expectedPath;
		
		String datasetUrl = null; 
		String targetUrl = "http://www.theworldavatar.com:83/kb/ontokin/ABF.owl";
		
		Object[] result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);

		String requestUrl = (String) result[0];
		assertEquals(expectedRequestUrl,requestUrl);
		URI uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		JSONObject joparams = (JSONObject) result[1];
		assertEquals("http://www.theworldavatar.com:83/kb/ontokin/ABF.owl",joparams.getString(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
		
		//////////////////////
		//targetUrl is a namespace
		targetUrl = "teststore";
		
		result =  AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrl,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(targetUrl,joparams.getString(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
	}

	@Test
	public void testCreateRequestUrlCase2() throws UnsupportedEncodingException, URISyntaxException {
		
		String datasetUrl;
		String targetUrl;
		String requestUrl;
		Object[] result;
		URI uri;
		JSONObject joparams;
		
		String expectedPath = JPSConstants.ACCESS_AGENT_PATH;
		String expectedRequestUrl =  "http://"+KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST)+expectedPath;
		
		//////////////////////
		//dataset, no target/graph
		datasetUrl = "http://www.theworldavatar.com:83/kb/ontokin"; 
		targetUrl = null;
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);

		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrl,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(datasetUrl,joparams.getString(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
				
		//////////////////////
		//dataset with graph
		datasetUrl = "http://www.theworldavatar.com:83/kb/ontokin"; 
		targetUrl = "http://www.theworldavatar.com:83/kb/ontokin/ABF.owl";
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrl,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(datasetUrl,joparams.getString(JPSConstants.TARGETIRI));
		assertEquals(targetUrl,joparams.getString(JPSConstants.TARGETGRAPH));
		
		//////////////////////
		//dataset is a namespace, no target/graph
		datasetUrl = "teststore"; 
		targetUrl = null;
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrl,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(datasetUrl,joparams.getString(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
	}
	
	@Test
	public void testCreateRequestUrlCase3() throws UnsupportedEncodingException, URISyntaxException {
		
		String datasetUrl;
		String targetUrl;
		String requestUrl;
		Object[] result;
		URI uri;
		JSONObject joparams;
		
		String scenarioUrl = "http://localhost:8080/jps/scenario/testScenario";
		JPSContext.putScenarioUrl(scenarioUrl);
		
		String expectedPath = "/jps/scenario/testScenario";
		String expectedRequestUrl = scenarioUrl;
		String expectedRequestUrlConverted = ResourcePathConverter.convert(expectedRequestUrl);
		
		//////////////////////
		//scenario, dataset, target
		datasetUrl = "http://www.theworldavatar.com/kb/ontokin"; 
		targetUrl = "http://www.theworldavatar.com/kb/ontokin/ABF.owl";
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrlConverted,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(targetUrl,joparams.getString(JPSConstants.SCENARIO_RESOURCE));
		assertEquals(datasetUrl,joparams.getString(JPSConstants.SCENARIO_DATASET));
		assertTrue(joparams.isNull(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
		
		///////////////////////
		//scenario, dataset, no target
		datasetUrl = "http://www.theworldavatar.com/kb/ontokin"; 
		targetUrl = null;
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrlConverted,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(datasetUrl,joparams.getString(JPSConstants.SCENARIO_RESOURCE));
		assertTrue(joparams.isNull(JPSConstants.SCENARIO_DATASET));
		assertTrue(joparams.isNull(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
		
		///////////////////////
		//scenario, no dataset, target
		datasetUrl = null; 
		targetUrl = "http://www.theworldavatar.com/kb/ontokin/ABF.owl";
		result = AccessAgentCaller.createRequestUrl(datasetUrl, targetUrl);
		assertNotNull(result);
		
		requestUrl = (String) result[0];
		assertEquals(expectedRequestUrlConverted,requestUrl);
		uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
		assertEquals(expectedPath,uri.getPath());
		
		joparams = (JSONObject) result[1];
		assertEquals(targetUrl,joparams.getString(JPSConstants.SCENARIO_RESOURCE));
		assertTrue(joparams.isNull(JPSConstants.SCENARIO_DATASET));
		assertTrue(joparams.isNull(JPSConstants.TARGETIRI));
		assertTrue(joparams.isNull(JPSConstants.TARGETGRAPH));
	}
	
	@Test
	public void testCutHashFragment() {
		
		String url;
		String result;
		
		url = "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl%23Service";
		result = AccessAgentCaller.cutHashFragment(url);
		assertEquals("http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl",result);
				
		url = "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service";
		result = AccessAgentCaller.cutHashFragment(url);
		assertEquals("http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl",result);
		
		url = "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl";
		result = AccessAgentCaller.cutHashFragment(url);
		assertEquals("http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl",result);
		
		url = "www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl%23Service";
		result = AccessAgentCaller.cutHashFragment(url);
		assertEquals("www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl",result);
		
		url = null;
		result = AccessAgentCaller.cutHashFragment(url);
		assertEquals((String) null,result);
	}

	@Test 
	public void testGetBaseWorldUrl() {
		
		String url;
		String result;
		String expected = "http://"+KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST)+JPSConstants.ACCESS_AGENT_PATH;
		String expectedLocal = "http://localhost:8080"+JPSConstants.ACCESS_AGENT_PATH;
		
		url = "http://www.theworldavatar.com:83/kb/agents/Service__OpenWeatherMap.owl%23Service";
		result = AccessAgentCaller.getBaseWorldUrl(url);
		assertEquals(expected,result);
	
		url = "http://www.theworldavatar.com:83/kb/ontokin";
		result = AccessAgentCaller.getBaseWorldUrl(url);
		assertEquals(expected,result);
		
		url = "http://localhost:8080/kb/ontokin";
		result = AccessAgentCaller.getBaseWorldUrl(url);
		assertEquals(expectedLocal,result);
		
		url = "ontokin";
		result = AccessAgentCaller.getBaseWorldUrl(url);
		assertEquals(expected,result);
	}
}
