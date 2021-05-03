package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;

public class WeatherIrradiationRetrieverTest{
	
	
	private static String ENIRI=null;
	private String baseUrl = null;
	private String irioftempS=null;
	private String iriofirrS=null;
	private String iriofwindS=null;
	
    @Before
    public void setUp() {
    	ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
    	baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
    	irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
        iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
        
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
	
	
}
