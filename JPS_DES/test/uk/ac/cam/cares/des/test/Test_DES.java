package uk.ac.cam.cares.des.test;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;


public class Test_DES extends TestCase{
	
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporepowernetwork/SingaporePowerNetwork.owl#SingaporePowerNetwork";
	
	public void testrunpython() throws IOException {
//		DistributedEnergySystem a = new DistributedEnergySystem();
//		String dataPath = QueryBroker.getLocalDataPath();
//		String baseUrl = dataPath + "/JPS_DES";
//		a.runOptimization(baseUrl);
		Runtime rt = Runtime.getRuntime();
		Process pr = rt.exec("python D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python", null, new File("D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python"));
	}

	public void testStartDESScenario() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		
//		String scenarioUrl = BucketHelper.getScenarioUrl("testtest");
//		
//		
//		JPSContext.putScenarioUrl(jo, scenarioUrl);
//		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
//		JPSContext.putUsecaseUrl(jo, usecaseUrl);
//		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	
	public void testIrradiationRetreiver() throws Exception {
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_DES";
		
		JSONObject jo = new JSONObject();
		
		jo.put("folder", baseUrl);
		jo.put("tempsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
		jo.put("speedsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		jo.put("irradiationsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		jo.put("jpscontext", "base");
		WeatherIrradiationRetriever a= new WeatherIrradiationRetriever();

		//a.readWritedatatoOWL(baseUrl,"http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
	}
	
	public void testcsvmanipulation () {
		String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";
		
		String iriirradiationsensor="http://localhost:8080/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
		String result2 = new QueryBroker().queryFile(iriirradiationsensor, sensorinfo2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		System.out.println("sizeofresult="+resultListfromqueryirr.size());
		System.out.println("element= "+resultListfromqueryirr.get(0)[2]);
		String content=resultListfromqueryirr.get(48)[2];
		System.out.println("year= "+content.split("#")[1].split("-")[0]);
		System.out.println("month= "+content.split("#")[1].split("-")[1]);
		System.out.println("date= "+content.split("#")[1].split("-")[2].split("T")[0]);
		System.out.println("time= "+content.split("#")[1].split("-")[2].split("T")[1].split("\\+")[0]);
	}
	
	
}
