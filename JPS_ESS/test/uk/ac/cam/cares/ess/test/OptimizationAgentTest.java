package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.ess.OptimizationAgent;

public class OptimizationAgentTest {
	private String storageIRI = null;
	
	@Before
	public void setUp() {
		storageIRI = "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
			
	}
	
	/** add validateInput() for OptimizationAgent
	 * 
	 */
	@Test
	public void testValidateInputOptimizationAgent() {
		JSONObject jo = new JSONObject();
		jo.put("storage", storageIRI);
		assertTrue(new OptimizationAgent().validateInput(jo));
		
	}
	
	/** test OptimizationAgent as itself
	 * 
	 */
	@Test
	public void testOptimizationAgent() {
		JSONObject jo = new JSONObject();
		jo.put("storage", storageIRI);
		JSONObject jo2 = new OptimizationAgent().processRequestParameters(jo);
		System.out.println(jo2.toString());
		String result2 = AgentCaller.executeGetWithJsonParameter( "JPS_ESS/OptimizationAgent", jo.toString());
		System.out.println(result2);	
		assertEquals(result2, jo2.toString());
	}
	
	
	
}
