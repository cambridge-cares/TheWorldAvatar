package uk.ac.cam.cares.jps.base.router.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.interfaces.CacheInterface;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.router.AgentRouter;

class AgentRouterTest {

	private final String defaultEndpoint = KeyValueMap.getInstance().get(IKeys.URL_AGENTROUTER_ENDPOINT);
	
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
		assertEquals(defaultEndpoint, (String) field.get(agentRouter));
				
		//change endpoint
		agentRouter.setRouterEndpoint(endpoint);	
		assertEquals(endpoint, (String) field.get(agentRouter));
		
		//reset default endpoint 
		agentRouter.setRouterEndpoint(defaultEndpoint);
	}
	
	@Test
	void testGetStoreClient() throws NoSuchFieldException, SecurityException, NoSuchMethodException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		
		AgentRouter agentRouter = AgentRouter.getInstance();
		
		assertNotNull(agentRouter.getClass().getDeclaredMethod("getStoreClient"));
	    Method method = agentRouter.getClass().getDeclaredMethod("getStoreClient");
	    method.setAccessible(true);
		
	    Object obj = method.invoke(agentRouter);
		assertNotNull(obj);
		assertTrue(obj.getClass().getClass().isInstance(RemoteStoreClient.class));
		
		StoreClientInterface storeClient = (RemoteStoreClient) obj;
		assertEquals(defaultEndpoint, storeClient.getQueryEndpoint());
		assertEquals(null, storeClient.getUpdateEndpoint());
	}
	
	@Test
	void testGetFromStore() {
		//TODO use a MockStoreClient
		fail("Not yet implemented");
	}
	
	@Test
	void testGet() {
		//TODO use spy to test
		fail("Not yet implemented");
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
