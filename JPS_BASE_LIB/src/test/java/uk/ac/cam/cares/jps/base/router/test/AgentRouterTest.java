package uk.ac.cam.cares.jps.base.router.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.router.AgentRouter;

class AgentRouterTest {

	private final String defaultEndpoint = KeyValueMap.getInstance().get(IKeys.URL_AGENTROUTER_ENDPOINT);
	
	private static String agentName1 = "Agent1";
	private static String agentURL1 = "http://www.example.com/agent1/location1";
	private static String agentName2 = "Agent2";
	private static String agentURL2 = "http://www.example.com/agent2/location2";
	private static String agentName3 = "Agent3";
	private static String agentURL3 = "http://www.example.com/agent3/location3";
	
	private static TripleStoreClientInterface createMockStore() {
		
		MockStoreClient mockStore = new MockStoreClient();
		mockStore.addTriple(		
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName1+".owl#Operation_1>",
				"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>");
		mockStore.addTriple(
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName1+".owl#Operation_1>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>",
				agentURL1);
		
		mockStore.addTriple(		
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName2+".owl#Operation_2>",
				"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>");
		mockStore.addTriple(
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName2+".owl#Operation_2>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>",
				agentURL2);
		
		mockStore.addTriple(		
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName3+".owl#Operation_3>",
				"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>");
		mockStore.addTriple(
				"<http://www.theworldavatar.com/kb/agents/Service_"+agentName3+".owl#Operation_3>",
				"<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>",
				agentURL3);
		
		return mockStore;	
	}
	
	////////////////////////////////////////
	
	@Test
	void testCacheInitialisation() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		assertNotNull(AgentRouter.getInstance());
		AgentRouter agentRouter = AgentRouter.getInstance();
				
		//assert cache instantiated
		assertNotNull(agentRouter.getClass().getSuperclass().getDeclaredField("cache"));
		Field cacheField = agentRouter.getClass().getSuperclass().getDeclaredField("cache");;
		cacheField.setAccessible(true);
		@SuppressWarnings("unchecked")
		CacheInterface<String, String> cache = (CacheInterface<String, String>) cacheField.get(agentRouter);
		assertNotNull(cache);
		assertTrue(cache.capacity()>0);
	}
	
	@Test
	void testSetRouterEndpoint() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		String endpoint = "http://www.example.com/test";
		AgentRouter agentRouter = AgentRouter.getInstance();
		
		assertNotNull(agentRouter.getClass().getDeclaredField("agentRouterEndpoint"));
		Field field = agentRouter.getClass().getDeclaredField("agentRouterEndpoint");
		field.setAccessible(true);
		
		//assert default endpoint is set
		agentRouter.resetRouterEndpoint();
		assertEquals(defaultEndpoint, (String) field.get(agentRouter));
				
		//change endpoint
		agentRouter.setRouterEndpoint(endpoint);	
		assertEquals(endpoint, (String) field.get(agentRouter));
		
		//reset default endpoint 
		agentRouter.resetRouterEndpoint();
		assertEquals(defaultEndpoint, (String) field.get(agentRouter));
	}
	
	@Test
	void testGetStoreClient() {
		
		AgentRouter agentRouter = AgentRouter.getInstance();
		agentRouter.resetRouterEndpoint();
		
	    Object obj = agentRouter.getStoreClient();
		assertNotNull(obj);
		assertTrue(obj.getClass().getClass().isInstance(RemoteStoreClient.class));

		TripleStoreClientInterface storeClient = (RemoteStoreClient) obj;
		assertEquals(defaultEndpoint, storeClient.getQueryEndpoint());
		assertEquals(null, storeClient.getUpdateEndpoint());
	}
	
	@Test
	void testGetFromStore() {

		TripleStoreClientInterface mockStoreClient = createMockStore();
				
		AgentRouter agentRouter = AgentRouter.getInstance();
		
	    String agentName = "Agent1";
	    String result = agentRouter.getFromStore(agentName, mockStoreClient);
		assertEquals(agentURL1, result);
		
		agentName = "Agent2";
		result = agentRouter.getFromStore(agentName, mockStoreClient);
		assertEquals(agentURL2, result);
		
		agentName = "Agent3";
		result = agentRouter.getFromStore(agentName, mockStoreClient);
		assertEquals(agentURL3, result);
	}
	
	@Test
	void testGetFromStoreException() {

		TripleStoreClientInterface storeClient = null;
	
		AgentRouter agentRouter = AgentRouter.getInstance();
		
		String agentName = "Agent1";		
	    Assertions.assertThrows(JPSRuntimeException.class, ()->{ agentRouter.getFromStore(agentName, storeClient);});
	}
	
	@Test
	void testGetFromStoreWithNull() {

		TripleStoreClientInterface mockStoreClient = createMockStore();
				
		AgentRouter agentRouter = AgentRouter.getInstance();
		
	    String agentName = "Agent4";
	    Object obj = agentRouter.getFromStore(agentName, mockStoreClient);
		assertNull(obj);
	}
	
	@Test
	void testGet() {

		TripleStoreClientInterface mockStoreClient = createMockStore();
		
		AgentRouter agentRouter = AgentRouter.getInstance();
		
		//Use mocked StoreClient as triple store
		AgentRouter spyAgentRouter = Mockito.spy(agentRouter);
		Mockito.doReturn(mockStoreClient).when(spyAgentRouter).getStoreClient();
		
		//Not in cache
		assertEquals(agentURL1, spyAgentRouter.get(agentName1));
		Mockito.verify(spyAgentRouter, Mockito.times(1)).getStoreClient();
		Mockito.verify(spyAgentRouter, Mockito.times(1)).getFromStore(agentName1, mockStoreClient);
		
		//In cache
		//Agent1 in cache so getFromStore not called again
		assertEquals(agentURL1, spyAgentRouter.get(agentName1));
		Mockito.verify(spyAgentRouter, Mockito.times(1)).getStoreClient();
		Mockito.verify(spyAgentRouter, Mockito.times(1)).getFromStore(agentName1, mockStoreClient);

		//Not in cache
		assertEquals(agentURL2, spyAgentRouter.get(agentName2));
		Mockito.verify(spyAgentRouter, Mockito.times(2)).getStoreClient();
		Mockito.verify(spyAgentRouter, Mockito.times(1)).getFromStore(agentName2, mockStoreClient);
		
		//Test Exception
		//Does not exist
		Assertions.assertThrows(JPSRuntimeException.class, ()->{spyAgentRouter.get("Agent4");});
		Mockito.verify(spyAgentRouter, Mockito.times(3)).getStoreClient();
		//Should try store again if null and called again
		Assertions.assertThrows(JPSRuntimeException.class, ()->{spyAgentRouter.get("Agent4");});
		Mockito.verify(spyAgentRouter, Mockito.times(4)).getStoreClient();
	}
	
	
	@Test
	void testGetQuery() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
		
		AgentRouter agentRouter = AgentRouter.getInstance();
		
		//assert variables
		String MSMhasHttpUrl = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>";
		String MSMOperation = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>";
		
		assertNotNull(agentRouter.getClass().getDeclaredField("MSM_HAS_HTTP_URL"));
		Field field = agentRouter.getClass().getDeclaredField("MSM_HAS_HTTP_URL");
		field.setAccessible(true);
		assertEquals(MSMhasHttpUrl, (String) field.get(agentRouter));
		
		assertNotNull(agentRouter.getClass().getDeclaredField("MSM_OPERATION"));
		field = agentRouter.getClass().getDeclaredField("MSM_OPERATION");
		field.setAccessible(true);
		assertEquals(MSMOperation, (String) field.get(agentRouter));
		
		//test query
		String testString = "test";
		String expected = "SELECT ?o"+
				"WHERE {"+
				"?s a "+MSMOperation+";"+
				" "+MSMhasHttpUrl+" ?o"+
				"FILTER contains(str(?s), \""+testString+"\")"+
				"}";		
		
		assertNotNull(agentRouter.getClass().getDeclaredMethod("getQuery", String.class));
	    Method method = agentRouter.getClass().getDeclaredMethod("getQuery", String.class);
	    method.setAccessible(true);
	    String query = (String) method.invoke(agentRouter,testString);
	    
	    assertEquals(expected.replaceAll("\\s",""), query.replaceAll("\\s",""));
	}
}
