package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
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
 * 
 * @author Laura Ong
 */
public class Test_DESWeatherAgents{
	
	
	private static String ENIRI=null;
	private String DISIRI=null;
	private String baseUrl = null;
	private String irioftempS=null;
	private String iriofirrS=null;
	private String irioftempF=null;
	private String iriofirrF=null;
	private String iriofwindS=null;
	
    @Before
    public void setUp() {
    	ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
    	DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
    	baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
    	irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
        iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
        iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
        irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		
    }
   

	/** test if GETREQ is in use. 
	 * @throws IOException 
	 * 
	 */
    @Test
	public void testWeatherForecast() throws IOException {

		long timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDayTemperature(irioftempF);
    	String fileStr = DESAgentNew.tempIRItoFile(irioftempF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDaySolcast(irioftempF,iriofirrF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	fileStr = DESAgentNew.tempIRItoFile(iriofirrF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
		
		
		
	}
	/**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * This test is disabled unless the entire process wants to be called. 
	 * 
	 */
    @Test
	public void testStartCoordinationDESScenariobase() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		jo.put("temperaturesensor", irioftempS);
    	jo.put("irradiationsensor",iriofirrS);
    	jo.put("windspeedsensor",iriofwindS);
		
		System.out.println(jo.toString());
		//Disabling this because we don't want solcast to execute each time 
		//We run a test
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESCoordination", jo.toString());
		
//		System.out.println(resultStart);
//		System.out.println("finished execute");

	}
	
	/** test if validateInput method is working in Weather Retriever
	 * 
	 */
    @Test
	public void testInputValidatorWeather() {
		JSONObject jo = new JSONObject()
				.put("electricalnetwork", ENIRI);
		jo.put("windspeedsensor", iriofwindS);
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
			WeatherIrradiationRetriever.readWritedatatoOWL(baseUrl,irioftempS,iriofirrS,iriofwindS);
			String destinationUrlWithoutHash = ScenarioHelper.cutHash(irioftempS);
			String fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
			assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
			destinationUrlWithoutHash = ScenarioHelper.cutHash(iriofirrS);
			fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
			assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
			destinationUrlWithoutHash = ScenarioHelper.cutHash(iriofwindS);
			fileStr = BucketHelper.getLocalPath(destinationUrlWithoutHash);
			assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
			// check that OWL is updated
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	/**
	 * Calls and runs the hourly weather retriever, that uses OCR (thru TOMCAT)
	 */
    @Test
	public void testIrradiationRetreiverAgentCall() throws Exception {
		JSONObject jo = new JSONObject();
		jo.put("windspeedsensor", iriofwindS);
		jo.put("temperaturesensor", irioftempS);
		jo.put("irradiationsensor", iriofirrS);
		jo.put("baseUrl", QueryBroker.getLocalDataPath()+"/JPS_DES");

		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
		System.out.println(resultStart);
		JSONObject v = new JSONObject(resultStart);
		assertNotNull(v.get("windspeedsensor"));
		assertNotNull(v.get("baseUrl"));
		assertTrue(InputValidator.checkIfValidIRI(v.getString("windspeedsensor")));
		assertTrue(InputValidator.checkIfValidFile(v.getString("baseUrl")));
	}
	/**
	 * Calls and runs the Blockchain transaction directly
	 */
    @Test
	public void testBlockchainWrapperDirectCall() throws IOException{
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
	

	

	
	
}
