package uk.ac.cam.cares.des.test;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;
import uk.ac.cam.cares.jps.des.n.CommercialAgent;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;
import uk.ac.cam.cares.jps.des.n.IndustrialAgent;
import uk.ac.cam.cares.jps.des.n.ResidentialAgent;
import uk.ac.cam.cares.jps.des.n.SolarAgent;

public class Test_DESSimulatorAgent extends TestCase{
	private String iriofnetworkdistrict =  "http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	private String irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
	private String iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
	private String iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	
	/** tests if Residential Agent calls successfully. 
	 * Residential Agent requires caresjpsutil library. 
	 * 
	 */
	public void testResidentialAgent() {
		new ResidentialAgent().extractResidentialData(iriofnetworkdistrict,"C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
		try {
			String result = new DESAgentNew().runPythonScript("residential.py", "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest");
			System.out.println(result);
			assertNotNull(result);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	/** test Residential agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
	public void testResidentialAgentCaller() {
		JSONObject jo = new JSONObject()
				.put("district",iriofnetworkdistrict);
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/ResidentialAgent", jo.toString());
		assertNotNull(resultStart);
	}
	/** tests of Commercial Agent runs successfully
	 * 
	 */
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
			ex.printStackTrace();
		}
	}
	/** test Commercial agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
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
			ex.printStackTrace();
		}
	}
	/** test Industrial agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
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
				ex.printStackTrace();
			}
	}
	/** test Solar agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
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
	public void testSystemAgent() {
		try {
			String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\Overall";
			
			new DESAgentNew().queryForIrradTemp(irioftempF,iriofirrF, baseUrl);
			OntModel model = DESAgentNew.readModelGreedy(iriofnetwork);
			new SolarAgent().provideGenlist(model, baseUrl); // create Parameters for Solar Cell
			new ResidentialAgent().extractResidentialData(iriofnetworkdistrict, baseUrl); //csv for residential
			new CommercialAgent().queryForBuildingConstants(model, baseUrl);;//csv for commercial
			new IndustrialAgent().queryForConstantsIndustrial(model, baseUrl);;//csv for commercial
			try {
				String result = new DESAgentNew().runPythonScript("system.py", baseUrl);
				assertNotNull(result);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} 
//			String result = new DESAgentNew().runPythonScript("system.py", "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\Overall");
//			System.out.println(result);
			}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	/** test System agent calls through Agent successfully
	 * dumps result in JPS Scenarios folder
	 */
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
		FrontEndTalk();
	}
	/** check if FrontEnd Case Scenario works
	 * 
	 */
	public void FrontEndTalk() {
		BlockchainWrapper bc = new BlockchainWrapper();
		//looks for last created directory through the Metadata Query
		String directorychosen= bc.getLastModifiedDirectory();
		System.out.println(directorychosen);
		//looks for the data according to the csvs stored
		JSONObject graData  = bc.provideJSONResult(directorychosen);
		JSONObject jo = bc.determineValue (graData);
		System.out.println(jo.toString());
		//TODO: later on, check value of data 
		assertNotNull(jo);
		
	}
	/** test if validateInput method is working in Commercial Agent
	 * 
	 */
	public void testInputValidatorResidential() {
		JSONObject jo = new JSONObject()
				.put("district",iriofnetworkdistrict);
		assertTrue(new ResidentialAgent().validateInput(jo));
		
		
	}
	/** test if validateInput method is working in Commercial Agent
	 * 
	 */
	public void testInputValidatorCommercial() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		assertFalse(new CommercialAgent().validateInput(jo));
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new CommercialAgent().validateInput(jo));
		
		
	}/** test if validateInput method is working in System Agent
	 * 
	 */
	public void testInputValidatorSystem() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", iriofnetwork);
		jo.put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertFalse(new DESAgentNew().validateInput(jo));
		jo.put("district",iriofnetworkdistrict);
		assertTrue(new DESAgentNew().validateInput(jo));
		
		
	}
	
}
