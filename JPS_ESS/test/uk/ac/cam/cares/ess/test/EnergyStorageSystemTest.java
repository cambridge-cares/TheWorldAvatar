package uk.ac.cam.cares.ess.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.ess.EnergyStorageSystem;


public class EnergyStorageSystemTest {
	private String ENIRI="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	private String batIRI="http://www.theworldavatar.com/kb/batterycatalog/BatteryCatalog.owl#BatteryCatalog";
	private String pvGenIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/PV-001.owl#PV-001";
	private String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\ESSTest";
	private String storageIRI = "http://www.jparksimulator.com/kb/batterycatalog/VRB.owl#VRB";
	List<String>pvgeniris= new ArrayList<String>();
	String usecaseID = UUID.randomUUID().toString();
	
	
	
	
	/** test validateInput() of Energy Storage System 
	 * 
	 */
	@Test
	public void testValidateInputEnergyStorageSystem() {
		JSONObject requestParam = new JSONObject().put("electricalnetwork", ENIRI);
		requestParam.put("BatteryCatalog", batIRI);
		assertTrue(new EnergyStorageSystem().validateInput(requestParam));
		
		
	}

	
	/** test filterPV method of EnergyStorageSystem
	 * 
	 */
	@Test
	public void testEnergyStorageSystemFilterPV() {
		List<String> ja = new EnergyStorageSystem().filterPV (ENIRI);
		assertEquals(ja.size(), 0); //There should be no photovoltaic generators attached to the EnergyStorageSystem at base
	}
	
	/** test prepareCSVPahigh of EnergyStorageSystem
	 * 
	 */
	public void testEnergyStorageSystemprepareCSVPahigh() {
		List<String> lstJA = new ArrayList<String>();
		lstJA.add(pvGenIRI);
		new EnergyStorageSystem(). prepareCSVPahigh( lstJA , baseUrl);
		File file = new File( baseUrl + "/Pa_high.csv");
		assertTrue(file.exists());
		assertTrue(file.length()> 0);
	}
	
	/** test prepareCSVRemaining of EnergyStorageSystem
	 * 
	 */
	@Test
	public void testEnergyStorageSystemprepareCSVRemaining() {
		
		new EnergyStorageSystem(). prepareCSVRemaining( batIRI, baseUrl );
		File file = new File( baseUrl + "/EnvironmentalScore.csv");
		assertTrue(file.exists());
		assertTrue(file.length()> 0); //That file is not empty
	}
	
	/** test readOutput of EnergyStorageSystem
	 * 
	 */
	@Test
	public void testreadsolutionstocsv() {
		String outputfiledir = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/solutions.csv";
		List<Double[]> simulationResult=new EnergyStorageSystem().readOutput(outputfiledir);

		System.out.println("simulation element = "+simulationResult.size());
		assertEquals(0.01,simulationResult.get(0)[0], 0.01);
		assertEquals(0.53,simulationResult.get(1)[1], 0.01);
		assertEquals(61.0,simulationResult.get(0)[2], 0.01);
		//ArrayList<String>removedplant=new ArrayList<String>();
		JSONObject result = new JSONObject();
	}	
	
	/** tests  modifyTemplate and runGAMS methods
	 * 
	 * @throws IOException
	 * @throws InterruptedException
	 */
	@Test
	public void testModifyTemplate() throws IOException, InterruptedException{
		EnergyStorageSystem a = new EnergyStorageSystem();
		a.runGAMS(baseUrl);
		  
	}
	
	/** test optimizedBatteryMatching() of EnergyStorageSystem ().
	 * 
	 * @throws IOException
	 */
	@Test
	public void testoptimizedbattery() throws IOException {
		
		
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		pvgeniris.add(pvGenIRI);
		JSONObject testres= new EnergyStorageSystem ().optimizedBatteryMatching(baseUrl, pvgeniris, batIRI);
		System.out.println("result battery= "+testres.getString("storage"));
		pvgeniris.clear();
		assertEquals(storageIRI, testres.getString("storage"));
		
	}
	
	
	@Test (expected = JPSRuntimeException.class)//As there is no renewable energy generator, don't expect this to run. 
	public void testEnergyStorageSystemAgentCall() throws IOException {
		
		JSONObject jo = new JSONObject()
				.put("BatteryCatalog", batIRI)
				.put("electricalnetwork", ENIRI);
		
		JSONObject testres= new EnergyStorageSystem ().processRequestParameters(jo);
		System.out.println("result battery= "+testres.getString("storage"));
		pvgeniris.clear();
		assertEquals(storageIRI, testres.getString("storage"));
		
	}
	
	
	
	
}
