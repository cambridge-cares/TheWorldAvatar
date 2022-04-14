package uk.ac.cam.cares.jps.base.agent.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.agent.AgentRouter;


class AgentRouterTest {

	@Test
	void testGetUrl() {
		String agentName = "PCE_Agent";
		AgentRouter agentRouter = new AgentRouter("http://localhost:48080/blazegraph/namespace/kb/sparql");
		String result = agentRouter.getUrl(agentName);
		assertEquals("http://kg.cmclinnovations.com:5001/api/model/predict",result);
	}
	
	@Test
	void testGetUrl2() {
		String agentName = "Test_Agent";
		AgentRouter agentRouter = new AgentRouter("http://localhost:48080/blazegraph/namespace/kb/sparql");
		String result = agentRouter.getUrl(agentName);
		assertEquals("http://kg.cmclinnovations.com:5001/api/model/predict",result);
	}
	
	@Test
	void testGetQuery() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
		
		AgentRouter agentRouter = new AgentRouter();
		
		//assert variables
		String MSMhasHttpUrl = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>";
		String MSMOperation = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>";
		
		assertNotNull(agentRouter.getClass().getDeclaredField("MSMhasHttpUrl"));
		Field field = agentRouter.getClass().getDeclaredField("MSMhasHttpUrl");
		field.setAccessible(true);
		assertEquals(MSMhasHttpUrl, (String) field.get(agentRouter));
		
		assertNotNull(agentRouter.getClass().getDeclaredField("MSMOperation"));
		field = agentRouter.getClass().getDeclaredField("MSMOperation");
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
