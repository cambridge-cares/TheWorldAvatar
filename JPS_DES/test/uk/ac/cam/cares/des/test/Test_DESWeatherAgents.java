package uk.ac.cam.cares.des.test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.ForecastAgent;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;
import uk.ac.cam.cares.jps.des.n.CommercialAgent;

/** Note that forecast agents are disabled in response to restriction
 * on number of solar calls
 * 
 * @author Laura Ong
 */
public class Test_DESWeatherAgents extends TestCase{
	
	private static String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	private String DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	private String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
	private String irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
    private String iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
    private String iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
    
    

	/** test if validateInput method is working in Forecast Agent
	 * @throws IOException 
	 * 
	 */
	public void testWeatherForecast() throws IOException {
		ForecastAgent a = new ForecastAgent();
		assertNotNull( ForecastAgent.GETReq(ENIRI));
		ArrayList<ArrayList<String>>  result = ForecastAgent.AccuRequest();
		assertNotNull(result.get(0).get(0));
		System.out.println(result.get(0).get(0));
		//Only enable this if the current test runs but Forecast Agent creates an error
		//Because we have a limited number of API calls
//		ArrayList<ArrayList<String>>  resultSun = ForecastAgent.SolCastRequest();
//		assertNotNull(resultSun.get(0).get(0));
		
		
		
	}
	/**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * This test is disabled unless the entire process wants to be called. 
	 * 
	 */
	public void xxtestStartCoordinationDESScenariobase() throws IOException  {
		

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
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		assertNotNull(new JSONObject(v).get("txHash"));
		assertNotNull(new JSONObject(v).get("sandr"));
	}
	

	

	
	
}
