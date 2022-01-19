package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
import uk.ac.cam.cares.jps.des.n.ResidentialAgent;

public class ResidentialAgentTest {
	
	private String iriofnetworkdistrict = null;
	
	
	@Before
	public void setUp() {
		iriofnetworkdistrict =  "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	
	}
	

	/** test if validateInput method is working in Residential Agent
	 * 
	 */
	@Test
	public void testInputValidatorResidential() {
		JSONObject jo = new JSONObject()
				.put("district",iriofnetworkdistrict);
		assertTrue(new ResidentialAgent().validateInput(jo));
		
		
	}
	

	/** tests if Residential Agent calls successfully. 
	 * Residential Agent requires caresjpsutil library. 
	 * 
	 */
	@Test
	public void testResidentialAgent() {
		new ResidentialAgent().extractResidentialData(iriofnetworkdistrict,"C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
		try {
			String result = new DESAgentNew().runPythonScript("residential.py", "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
			System.out.println(result);
			assertNotNull(result);
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("ResidentialAgent: Incomplete simulation.\n");
		}
	}
	
	/** test Residential agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
	@Test
	public void testResidentialAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("district",iriofnetworkdistrict);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/ResidentialAgent", jo.toString());
		assertNotNull(resultStart);
	}
	
}
