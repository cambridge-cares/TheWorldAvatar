package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.des.n.CommercialAgent;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

public class CommercialAgentTest {

	private String irioftempF= null;
	private String iriofirrF= null;
	private String iriofnetwork =  null;

	@Before
	public void setUp() {
		irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";

	}
	
	/** test if validateInput method is working in Commercial Agent
	 * 
	 */
	@Test
	public void testInputValidatorCommercial() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new CommercialAgent().validateInput(jo));
		
		
	}
	
	
	
	/** tests of Commercial Agent runs successfully
	 * 
	 */
	@Test
	public void testCommercialAgent() {
		String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\commercial";
	    new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
		OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
		new CommercialAgent().queryForBuildingConstants(model, baseUrl);
        try {
			String result = new DESAgentNew()
					.runPythonScript("commercial.py"
							, baseUrl);
			System.out.println(result);
			assertNotNull(result);
			}
		catch (Exception ex) {
			throw new JPSRuntimeException("CommercialAgent: Incomplete simulation.\n");
		}
	}
	
	/** test Commercial agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
	@Test
	public void testCommercialAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/CommercialAgent", jo.toString());
		assertNotNull(resultStart);
	}
	
	

}
