package uk.ac.cam.cares.jps.wte.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.List;
import java.util.UUID;

import org.apache.jena.ontology.OntModel;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.wte.FCQuerySource;
import uk.ac.cam.cares.jps.wte.WTECoordination;
import uk.ac.cam.cares.jps.wte.WTEProcessResult;
import uk.ac.cam.cares.jps.wte.WastetoEnergyAgent;

public class WTECoordinationTest {

	static String iriofnetwork=null;
	static String baseUrl = null;
	String usecaseID = null;
	
	@Before
	public void setUp() {
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";
		baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\WTETest";
		usecaseID = UUID.randomUUID().toString();
	}
	/** test WTECoordination validateInput method
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testInputValidatorWTECoordination() throws JSONException {
		
		JSONObject jo = new JSONObject();
		assertFalse(new WTECoordination().validateInput(jo));
		
	}
	
	/** enables scenarioName in testInSuccession
	 * 
	 * @param scenarioName
	 * @return
	 */
	public String enableScenario(String scenarioName) {
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		JPSHttpServlet.enableScenario(scenarioUrl);	
		return scenarioUrl;
	}
	
	/** Debug both WTEAgent and WTEKBCreator and WTEProcessResult in a scenario called testScenariosWithWTE
	 * This test is to directly pause at each step to figure out where the bug occurs; 
	 * And can be used to check if the file was created at the appropriate step in WasteToEnergyAgent
	 * @throws Exception
	 */
//	@Test
	public void testInSuccession() throws Exception {
		WastetoEnergyAgent ag = new WastetoEnergyAgent();
		enableScenario("testScenariosWithWTE");
		String baseUrl = QueryBroker.getLocalDataPath();
		OntModel model= FCQuerySource.readModelGreedy(iriofnetwork);
		ag.prepareCSVFC("Site_xy.csv","Waste.csv", baseUrl,model,15); 
		//test if Site_xy.csv is created (can't check if csv file is empty unfortunately)
		File file = new File(baseUrl +  "/Site_xy.csv");
		assertTrue(file.length() !=0);
		String n_cluster= "40";
        new QueryBroker().putLocal(baseUrl + "/n_cluster.txt",n_cluster ); 
		ag.prepareCSVWT("Location.csv", baseUrl,model); 
		file = new File(baseUrl +  "/Location.csv");
		assertTrue(file.length() !=0);
		ag.prepareCSVTransport(WastetoEnergyAgent.getTransportQuery(),"transport.csv", baseUrl,model); 

		file = new File(baseUrl +  "/transport.csv");
		assertTrue(file.length() !=0);
		ag.prepareCSVCompTECHBased(WastetoEnergyAgent.returnUpperBoundQuery(),baseUrl,model);
		String WTFTechOffsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OffsiteWasteTreatmentFacility").buildString();
		ag.prepareCSVTECHBased(WTFTechOffsiteQuery,baseUrl,model,"offsite");
		String WTFTechOnsiteQuery = FCQuerySource.getTechQuery() 
				.addWhere("?entity" ,"a", "j1:OnsiteWasteTreatmentFacility")
				.addWhere("?Tech1" ,"a", "j1:OnSiteDigester").buildString();
		ag.prepareCSVTECHBased(WTFTechOnsiteQuery,baseUrl,model,"onsite");
		//TODO: should be able to run Main.m without needing to copy over, but I don't know Matlab well enough how to avoid doing so. 
		// Should need something like fullfile to use "readmatrix" to read an absolute path. 
		ag.copyTemplate(baseUrl, "SphereDist.m");
		ag.copyTemplate(baseUrl, "Main.m");
		ag.copyTemplate(baseUrl, "D2R.m");		
		ag.createBat(baseUrl, n_cluster);
		System.out.println("Matlab simulation should have finished. ");
		//Set a breakpoint here, and wait for the matlab simulation to complete. 
//			Read for next agent
		WTEProcessResult at = new WTEProcessResult();
		List<String[]> resu =   FCQuerySource.queryResult(model,WastetoEnergyAgent.getFCQuery());
		List<String[]> fcMapping = at.createFoodCourt(resu);
		assertEquals(109, fcMapping.size());
		List<String[]> propertydataonsite = FCQuerySource.queryResult(model, WTFTechOnsiteQuery);
		assertEquals(propertydataonsite.get(0).length, 7);

		model= FCQuerySource.readModelGreedy(iriofnetwork);
		//While in the actual code this is meant to be a for loop of fifteen years, here in the test, we bother with creating
		// the needed onsite WTF for the current year. 
		List<String> onsiteiricomplete=at.updateinOnsiteWT(fcMapping,baseUrl,propertydataonsite,1);
		List<String[]> inputoffsitedata = at.readResult(baseUrl,"n_unit_max_offsite.csv");
		List<String[]> sitemapping = at.updateNewFC(baseUrl,inputoffsitedata );
		assertEquals(sitemapping.size(), 1635);//or 109*15 years
		at.updateFCHelper(sitemapping);
		at.updateKBForSystem(iriofnetwork, baseUrl, WTEProcessResult.getWasteSystemOutputQuery(),onsiteiricomplete); //for waste system	
		at.updateinOffsiteWT(inputoffsitedata,baseUrl, 15);
		
	}
	
	/** Run simulation via Scenario Client to get individual scenarios through agent
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testCreateScenarioAndCallWTEAgent() throws JSONException {
		
		String scenarioName = "testwaste2-"+usecaseID;
		iriofnetwork ="http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/SingaporeWasteSystem.owl#SingaporeWasteSystem";		
		String json = new JSONStringer().object()
				.key("wastenetwork").value(iriofnetwork)
				.key("n_cluster").value("40")
				.endObject().toString();
		String result = new ScenarioClient().call(scenarioName, "http://localhost:8080/JPS_WTE/startsimulationCoordinationWTE", json);
		assertNotNull(new JSONObject(result).getString("n_cluster")); //To check if there is a return from WTECoordinationAgent
		
	}
}
