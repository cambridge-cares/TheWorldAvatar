package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;

public class FrontEndCoordinationTest {

	public FrontEndCoordinationTest() {
		// TODO Auto-generated constructor stub
	}
	/** checks for empty input using validateInput() for FrontEnd Coordination Agent
	 * 
	 */
	@Test
	public void testInputValidatorFrontEndCoordination(){
		JSONObject jo = new JSONObject();
	    assertFalse(new FrontEndCoordination().validateInput(jo));	
	    jo.put("key", "value");
	    assertTrue(new FrontEndCoordination().validateInput(jo));		
	}
	
	
	/** test if FrontEndCoordination works if called through agent
	 * Assuming that a run was completed beforehand
	 */
	@Test
	public void testFrontEndCoordinationAgentCall() {
		JSONObject jo = new JSONObject().put("key", "value");
		JSONObject joRes = new JSONObject(AgentCaller
				.executeGetWithJsonParameter("JPS_DES/showDESResult",
						jo.toString()));
		assertNotNull(joRes.get("txHash"));
	}

}
