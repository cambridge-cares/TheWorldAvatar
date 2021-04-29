package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.n.CommercialAgent;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
import uk.ac.cam.cares.jps.des.n.IndustrialAgent;
import uk.ac.cam.cares.jps.des.n.ResidentialAgent;
import uk.ac.cam.cares.jps.des.n.SolarAgent;

/** Tests for CommercialAgent, DESAgentNew, IndustrialAgent, ResidentialAgent, SolarAgent
 * 
 *
 */
public class Test_DESSimulatorAgent{
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
	
	/** tests if Solar Radiation Agent calls successfully. 
	 * Solar Radiation Agent requires caresjpsutil library. 
	 *  
	 */
	@Test
	public void testSolarAgent() {
		SolarAgent sa = new SolarAgent();
		String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar";
		new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
        
		OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
		sa.provideGenlist(model, baseUrl);
		 try {
				String result = new DESAgentNew().runPythonScript("solarRadiation.py", baseUrl);
				System.out.println(result);
				assertNotNull(result);
				}
			catch (Exception ex) {
				throw new JPSRuntimeException("SolarAgent: Incomplete simulation.\n");
			}
	}
	
	/** test Solar agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
	@Test
	public void testSolarAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/SolarAgent", jo.toString());
		assertNotNull(resultStart);
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
	
	/** test if validateInput method is working in Residential Agent
	 * 
	 */
	@Test
	public void testInputValidatorResidential() {
		JSONObject jo = new JSONObject()
				.put("district",iriofnetworkdistrict);
		assertTrue(new ResidentialAgent().validateInput(jo));
		
		
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
	
	/** test if validateInput method is working in Solar Agent
	 * 
	 */
	@Test
	public void testInputValidatorSolar() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new SolarAgent().validateInput(jo));
		
		
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
	
	
}
