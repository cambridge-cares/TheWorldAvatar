package uk.ac.cam.cares.jps.powsys.carbontax.test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.powsys.carbontax.CarbonTaxAgent;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.test.TestEN;

public class TestCarbonTaxAgent extends TestCase {
	CarbonTaxAgent a= new CarbonTaxAgent();
	public void testLocalRun() throws IOException, InterruptedException { //warning, need to put owl file in root localhost
		
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		System.out.println("what is dataPath="+dataPath);
		
		a.prepareCSVGeneratorParameterUpdatedGenScale(iriofnetwork,dataPath);
		String filename="time_profile.csv";
		a.copyTemplate(dataPath, filename);
		//a.copyTemplate(dataPath, "Generator_Parameters.csv");

		
		BigDecimal c;
		c= new BigDecimal("170"); 
		a.prepareConstantCSV(c,dataPath);
		a.runGAMS(dataPath);
		File file = new File(dataPath+"/results.csv");
		assertTrue(file.exists());
		
	}
	
	public void xxxtestGAMSRun() throws IOException, InterruptedException { //only to test the gums code if it's running automatically
		a.runGAMS("C:/JPS_DATA/workingdir/JPS_SCENARIO/scenario/base/localhost_8080/data/06bc4a0f-f92a-4c59-88c5-eb0bf1f3c978");
	}
	
	public void testCSVReactorParameter() throws IOException, InterruptedException { //warning, need to put owl file in root localhost
		
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		System.out.println("what is dataPath="+dataPath);
		CarbonTaxAgent a= new CarbonTaxAgent();
		
		//a.prepareCSVGeneratorParameter(iriofnetwork,dataPath);
		a.prepareCSVGeneratorParameterUpdatedGenScale(iriofnetwork,dataPath);
		//expectation that the csv file should be created correctly with 29 gen
		File file = new File(dataPath+"/Generator_Parameters.csv");
		assertTrue(file.exists());
		
	}
	
	public void testCallCarbonTax() {
		String scenarioName = "testPOWSYSCarbonTaxLocalserver";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		System.out.println(scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		jo.put("carbontax",new BigDecimal("170") );
		String resultProcess=AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/optimizeforcarbontax", jo.toString());
		System.out.println("output= "+resultProcess);
		assertNotNull(new JSONObject(resultProcess).get("substitutionalgenerators"));
	}
	/** test validateInput() of CarbonTaxAgent
	 * 
	 */
	public void testInputValidatorCarbonTax() {
		JSONObject jo = new JSONObject();
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		jo.put("electricalnetwork", iriofnetwork);
		jo.put("carbontax", new BigDecimal("170"));
		assertTrue(a.validateInput(jo));
		
	}
}
