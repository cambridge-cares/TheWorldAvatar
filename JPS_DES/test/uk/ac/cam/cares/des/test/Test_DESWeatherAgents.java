package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Date;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.ForecastAgent;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

/** Note that forecast agents are disabled in response to restriction
 * on number of solar calls
 * Tests for WeatherIrradiationRetriever, BlockchainWrapper, Forecast Agent, and Front End Coordination Agent. 
 */
public class Test_DESWeatherAgents{
	
	
	private static String ENIRI=null;
	private String baseUrl = null;
	private String irioftempS=null;
	private String iriofirrS=null;
	private String irioftempF=null;
	private String iriofirrF=null;
	private String iriofwindS=null;
	
    @Before
    public void setUp() {
    	ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
    	baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
    	irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
        iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
        irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		
    }
    /** test if validateInput method is working in Weather Retriever
	 * 
	 */
    @Test
	public void testInputValidatorForecast() {
		JSONObject jo = new JSONObject().put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new ForecastAgent().validateInput(jo));		
	}

	/** This tests if the forecast was ran correctly when called directly.  
	 * This test should not be run due to the limited number of calls alloted to a free account. 
	 * @throws IOException 
	 * 
	 */
//    @Test
	public void testWeatherForecast() throws IOException {

		long timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDaySolcast(irioftempF,iriofirrF);
    	String fileStr = DESAgentNew.tempIRItoFile(iriofirrF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	fileStr = DESAgentNew.tempIRItoFile(irioftempF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDayTemperature(irioftempF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));		
	}
	
	/** test if validateInput method is working in Weather Retriever
	 * 
	 */
    @Test
	public void testInputValidatorWeather() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", ENIRI);
		jo.put("temperaturesensor", irioftempS);
		jo.put("irradiationsensor", iriofirrS);
		assertTrue(new WeatherIrradiationRetriever().validateInput(jo));		
	}
    
	/**
	 * Calls and runs the hourly weather retriever, that uses OCR
	 */
    @Test
	public void testWeatherIrradiationDirect() {
		long timeLast = new Date().getTime();
		try {
			WeatherIrradiationRetriever.readWritedatatoOWL(baseUrl,irioftempS,iriofirrS);
			String destinationUrlWithoutHash = ScenarioHelper.cutHash(irioftempS);
			String fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
			assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
			destinationUrlWithoutHash = ScenarioHelper.cutHash(iriofirrS);
			fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
			assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
			// check that OWL is updated
		} catch (Exception e) {
			throw new JPSRuntimeException("");
		}
	}
    
	/** Calls and runs the hourly weather retriever, that uses OCR (thru Server run)
	 *
	 */
    @Test
	public void testIrradiationRetrieverAgentCall() throws Exception {
		long timeLast = new Date().getTime();
		JSONObject jo = new JSONObject();
		jo.put("windspeedsensor", iriofwindS);
		jo.put("temperaturesensor", irioftempS);
		jo.put("irradiationsensor", iriofirrS);
		jo.put("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES");

		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
		System.out.println(resultStart);
		JSONObject v = new JSONObject(resultStart);
		assertNotNull(v.get("baseUrl"));
		assertTrue(InputValidator.checkIfValidFile(v.getString("baseUrl")));
		String destinationUrlWithoutHash = ScenarioHelper.cutHash(irioftempS);
		String fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
		assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
		destinationUrlWithoutHash = ScenarioHelper.cutHash(iriofirrS);
		fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
		assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
		destinationUrlWithoutHash = ScenarioHelper.cutHash(iriofwindS);
		fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
		assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
	}
    /** checks for empty input using validateInput() for FrontEnd Coordination Agent
	 * 
	 */
	@Test
	public void testInputValidatorFrontEndCoordination(){
		JSONObject jo = new JSONObject();
	    assertFalse(new BlockchainWrapper().validateInput(jo));	
	    jo.put("key", "value");
	    assertTrue(new BlockchainWrapper().validateInput(jo));		
	}
	
	/** checks for empty input using validateInput() for BlockchainWrapper Agent
	 * 
	 */
	@Test
	public void testInputValidatorBlockchainWrapper(){
		JSONObject jo = new JSONObject();
	    assertFalse(new BlockchainWrapper().validateInput(jo));	
	    jo.put("key", "value");
	    assertTrue(new BlockchainWrapper().validateInput(jo));	
	}
	
    
    /** test loadProperties in BlockchainWrapper
     * 
     */
    @Test
    public void testgetPropertiesFromBlockchainWrapper() {
    	BlockchainWrapper ab = new BlockchainWrapper();
    	assertNotNull(ab.addrOfI, "industrial.json");
    }
    
	/**
	 * Calls and runs the Blockchain transaction directly
	 */
    @Test
	public void testBlockchainWrappercalculateTrade() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		JSONObject v = new BlockchainWrapper().calculateTrade(jo);
		assertNotNull(v.get("txHash"));
		assertNotNull(v.get("sandr"));
	}

	/**
	 * Calls and runs the Blockchain transaction using Agent
	 */
    @Test
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		assertNotNull(new JSONObject(v).get("txHash"));
		assertNotNull(new JSONObject(v).get("sandr"));
	}
    
	
	/** test if Blockchain Wrapper works if called directly
	 * Assuming that a run was completed beforehand
	 */
	@Test
	public void testBlockchainWrapperDirect() {
		BlockchainWrapper bc = new BlockchainWrapper();
		//looks for last created directory through the Metadata Query
		String directorychosen= bc.getLastModifiedDirectory();
		System.out.println(directorychosen);
		//looks for the data according to the csvs stored
		JSONObject graData  = bc.provideJSONResult(directorychosen);
		JSONObject jo = bc.determineValue (graData);
		System.out.println(jo.toString());
		assertNotNull(jo);
		JSONObject result = bc.calculateTrade(jo);
		assertNotNull(result.get("txHash"));
		assertNotNull(result.get("sandr"));
		
	}
	
	/** test if FrontEndCoordination works if called through agent
	 * Assuming that a run was completed beforehand
	 */
	@Test
	public void testFrontEndCoordinationAgentCall() {
		JSONObject jo = new JSONObject().put("key", "value");
		JSONObject joRes = new JSONObject(AgentCaller
				.executeGetWithJsonParameter("JPS_DES/showDESResult",
						jo.toString()));
		assertNotNull(joRes.get("txHash"));
	}

    /**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * This test should not be run due to the limited number of calls alloted to a free account. 
	 * This test should not return a result
	 */
    @Test
	public void testStartCoordinationDES() throws IOException  {
		JSONObject jo = new JSONObject();	
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESCoordination", jo.toString());
		
	}
	
	

	
	
}
