package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.n.CommercialAgent;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
import uk.ac.cam.cares.jps.des.n.IndustrialAgent;
import uk.ac.cam.cares.jps.des.n.ResidentialAgent;
import uk.ac.cam.cares.jps.des.n.SolarAgent;

public class DESSystemAgentTest {

	private String iriofnetworkdistrict = null;
	private String irioftempF= null;
	private String iriofirrF= null;
	private String iriofnetwork =  null;
	
	
	@Before
	public void setUp() {
		iriofnetworkdistrict =  "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
		irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
		
		
	}
	
	/** test if validateInput method is working in System Agent
	 * 
	 */
	@Test
	public void testInputValidatorSystem() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		jo.put("district",iriofnetworkdistrict);
		assertTrue(new DESAgentNew().validateInput(jo));
		
		
	}
	
	/** tests if System Agent calls successfully. 
	 * System Agent requires caresjpsutil library. 
	 * 
	 */
	@Test
	public void testSystemAgent() {
			String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\Overall";
			
			new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
			OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
			new SolarAgent().provideGenlist(model, baseUrl); // create Parameters for Solar Cell
			new ResidentialAgent().extractResidentialData(iriofnetworkdistrict, baseUrl); //csv for residential
			new CommercialAgent().queryForBuildingConstants(model, baseUrl);;//csv for commercial
			new IndustrialAgent().queryForConstantsIndustrial(model, baseUrl);;//csv for commercial
			String result = new DESAgentNew().runPythonScript("system.py", baseUrl);
			assertNotNull(result);			
	}
	
	/** test System agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 * Calls FrontEndTalk to check results of BlockchainWrapper
	 */
	@Test
	public void testSystemAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		jo.put("district",iriofnetworkdistrict);
	        
		jo.put("cityIRI", "http://dbpedia.org/page/Singapore");
	    jo.put("baseUrl",  QueryBroker.getLocalDataPath()+"/JPS_DES");
	        
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgentNew", jo.toString());
		assertNotNull(resultStart);
	}	
	
}
