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

/** Tests for CommercialAgent, DESAgentNew, ResidentialAgent, SolarAgent
 * 
 *
 */
public class SolarAgentTest{
	private String irioftempF= null;
	private String iriofirrF= null;
	private String iriofnetwork =  null;
	
	
	@Before
	public void setUp() {
		irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		iriofnetwork = "http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
		
		
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
	
	
	
	
}
