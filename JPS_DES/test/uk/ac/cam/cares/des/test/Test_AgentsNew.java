package uk.ac.cam.cares.des.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.des.ResidentialAgent;

public class Test_AgentsNew extends TestCase{
	/** tests if Residential Agent calls successfully. 
	 * Residential Agent requires caresjpsutil library. 
	 * 
	 */
	public void testResidentialAgent() {
		String iriofnetworkdistrict =  "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
		new ResidentialAgent().extractResidentialData(iriofnetworkdistrict,"C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
		try {
			String result = new ResidentialAgent().runResidentialPy("residential.py", "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
			System.out.println(result);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	public void testResidentialAgentCaller() {
		JSONObject jo = new JSONObject().put("district", "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/ResidentialAgent", jo.toString());
		assertNotNull(resultStart);
	}
}
