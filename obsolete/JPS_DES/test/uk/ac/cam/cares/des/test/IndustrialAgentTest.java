package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
import uk.ac.cam.cares.jps.des.n.IndustrialAgent;

public class IndustrialAgentTest {

	private String irioftempF= null;
	private String iriofirrF= null;
	private String iriofnetwork =  null;

	@Before
	public void setUp() {
		irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
		
		
	}
	
	/** test if validateInput method is working in Industrial Agent
	 * 
	 */
	@Test
	public void testInputValidatorIndustrial() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new IndustrialAgent().validateInput(jo));
		
		
	}
	
	/** test Industrial agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
	@Test
	public void testIndustrialAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/IndustrialAgent", jo.toString());
		assertNotNull(resultStart);
	}
	
	/** tests if Industrial Agent calls successfully. 
	 * Industrial Agent requires caresjpsutil library. 
	 * 
	 */
	@Test
	public void testIndustrialAgent() {
	    String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\industrial";
		 
		new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        
        OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
		IndustrialAgent ic = new IndustrialAgent();
		ic.queryForChemicalConstants(model, baseUrl);
        ic.queryForFuelCellConstants(model, baseUrl);
		
       try {
			String result = new DESAgentNew().runPythonScript("industrial.py", baseUrl);
			System.out.println(result);
			assertNotNull(result);
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("IndustrialAgent: Incomplete simulation.\n");
		}
	}

}
