package uk.ac.cam.cares.des.test;

import java.io.IOException;
import org.json.JSONObject;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;


public class Test_DES extends TestCase{
	
	private static String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	private String DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	
	/**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * 
	 */
	public void testStartCoordinationDESScenariobase() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		jo.put("temperaturesensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
    	jo.put("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
    	jo.put("windspeedsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESCoordination", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	
	/**
	 * Calls upon the FrontEnd Coordination agent that would call the latest DES run (Forecast+DESpython wrapper)
	 * And afterwards blockchain wrapper
	 */
	public void testStartDESScenariobaseshowingresult() throws IOException  { //must have at least 1 directory with complete running first to make it success
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/showDESResult", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	

	/**
	 * Calls and runs the hourly weather retriever, that uses OCR
	 */
	public void testWeatherIrradiationDirect() {
		String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\DESTest\\solar2";
		String irioftempS="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001";
	    String iriofirrS="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
	    String iriofwindS="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001";
	    
		try {
			WeatherIrradiationRetriever.readWritedatatoOWL(baseUrl,irioftempS,iriofirrS,iriofwindS);
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
		
		jo.put("folder", QueryBroker.getLocalDataPath()+"/JPS_DES");
		jo.put("tempsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
		jo.put("speedsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		jo.put("irradiationsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		jo.put("jpscontext", "base");

		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
		System.out.println(resultStart);
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
		System.out.println(new BlockchainWrapper().calculateTrade(jo));
	}

	/**
	 * Calls and runs the Blockchain transaction using Agent
	 */
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		jo.put("directory",new FrontEndCoordination().getLastModifiedDirectory());
	    System.out.println(jo.toString());
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		System.out.println(v);
	}
	

	

	
	
}
