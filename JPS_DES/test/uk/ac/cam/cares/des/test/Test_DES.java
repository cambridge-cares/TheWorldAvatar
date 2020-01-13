package uk.ac.cam.cares.des.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.des.DistributedEnergySystem;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;


public class Test_DES extends TestCase{
	
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalnetwork.owl#SingaporeElectricalnetwork";
	private String DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	
	public void testrunpython() throws IOException {
//		DistributedEnergySystem a = new DistributedEnergySystem();
//		String dataPath = QueryBroker.getLocalDataPath();
//		String baseUrl = dataPath + "/JPS_DES";
//		a.runOptimization(baseUrl);
		Runtime rt = Runtime.getRuntime();
		Process pr = rt.exec("python D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python", null, new File("D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python"));
	}
	
	public void testrunpython2() throws IOException {
//		DistributedEnergySystem a = new DistributedEnergySystem();
//		String dataPath = QueryBroker.getLocalDataPath();
//		String baseUrl = dataPath + "/JPS_DES";
//		a.runOptimization(baseUrl);
		Runtime rt = Runtime.getRuntime();
		int returnValue = -1;
		System.out.println("Working Directory = " + System.getProperty("user.dir"));
		Process pr = rt.exec("python D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python\\ocrv1.py", null, new File("D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\python"));
		try {
			pr.waitFor();
			returnValue = pr.exitValue();
		} catch (InterruptedException e) {
			e.printStackTrace();
			System.out.println(e);
		}
			System.out.println(returnValue);
		}

	public void testStartDESScenariobase() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	public void testStartDESScenariotemp() throws IOException  {

		DistributedEnergySystem a = new DistributedEnergySystem();
        String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\8f039efb-f0a1-423a-afc8-d8a32021e8e7\\JPS_DES"; //successful result
		String iriofnetwork = ENIRI;
		String iriofdistrict = DISIRI;
		OntModel model = readModelGreedy(iriofnetwork);
		
		a.extractResidentialData(iriofdistrict, baseUrl); //csv for residential
		String weatherdir=baseUrl+"/Weather.csv";
		String content = new QueryBroker().readFileLocal(weatherdir);
		List<String[]> weatherResult = MatrixConverter.fromCsvToArray(content);
		
		String powerdir=baseUrl+"/totgen.csv";
		String content2 = new QueryBroker().readFileLocal(powerdir);
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(content2);
		
		String rhdir=baseUrl+"/rh1.csv";
		String content3 = new QueryBroker().readFileLocal(rhdir);
		List<String[]> rhResult = MatrixConverter.fromCsvToArray(content3);

		JSONArray temperature=new JSONArray();
		JSONArray irradiation=new JSONArray();
		JSONObject dataresult= new JSONObject();

		
		//25-48 (last 24)
		int sizeofweather=weatherResult.size();
		for (int x=sizeofweather-24;x<sizeofweather;x++) {
			temperature.put(weatherResult.get(x)[4]);
			irradiation.put(weatherResult.get(x)[8]);
		}
		//log to check if it's reading the right one. x
		
		dataresult.put("temperature", temperature);
		dataresult.put("irradiation", irradiation);
		dataresult.put("fuelcell", simulationResult.get(3));
		dataresult.put("residential", simulationResult.get(0));
		dataresult.put("industrial", simulationResult.get(2));
		dataresult.put("building", simulationResult.get(1));
		dataresult.put("rh1",rhResult.subList(0, 3).toArray());
		dataresult.put("rh2",rhResult.subList(3, 6).toArray());
		dataresult.put("rh3",rhResult.subList(6, rhResult.size()).toArray());
		
		System.out.println("result: "+dataresult.toString());
	}
	public void testStartDESScenario() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testDES");
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
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

		a.readWritedatatoOWL(baseUrl,"http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
//		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetIrradiationandWeatherData", jo.toString());
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
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE { " 
				+ "?entity   j2:hasSubsystem ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	
	public static OntModel readModelGreedyForUser(String useriri) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "SELECT ?component "
				+ "WHERE { " 
				+ "?entity   j2:isConnectedTo ?component ." 
				+ "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(useriri, electricalnodeInfo);
	}
	
	public void testquerygreedymultiple() { //testing for csv creation related to residential
		String iriofnetworkdistrict="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
		OntModel model = readModelGreedy(iriofnetworkdistrict);	
		String groupInfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "SELECT DISTINCT ?entity (COUNT(?entity) AS ?group) ?propval ?user "
				+ "WHERE {"
				+ "{ ?entity a j6:Building ."  
				+ "  ?entity j2:hasProperty ?prop ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ."
				+ "?entity j4:isComprisedOf ?user ."	
				+ "}"
				+"FILTER regex(STR(?user),\"001\") ."
				+ "}" 
				+ "GROUP BY ?entity ?propval ?user "; 
		
		
		
		String groupInfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "SELECT DISTINCT ?entity (COUNT(?entity) AS ?group) "
				+ "WHERE "
				+ "{ ?entity a j6:Building ."
				+ "?entity j4:isComprisedOf ?user ."	 
			
				+ "}"

 
				+ "GROUP BY ?entity "; 
		
		String equipmentinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j7:<http://www.w3.org/2006/time#> "
				 + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "SELECT ?entity ?Pmaxval ?Pminval ?unwillval ?Pactval ?hourval "
				+ "WHERE "
				+ "{ ?entity a j6:Electronics ."
				+ "?entity j9:hasActivePowerAbsorbed ?Pmax ."
				+ "?Pmax a j9:MaximumActivePower ."
				+ " ?Pmax   j2:hasValue ?vPmax ."
				+ " ?vPmax   j2:numericalValue ?Pmaxval ."
				
				+ "  ?entity j2:hasProperty ?prop ."
				+ "?prop a j6:IdealityFactor ."
				+ " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?unwillval ."
				
				+ "?entity j9:hasActivePowerAbsorbed ?Pmin ."
				+ "?Pmin a j9:MinimumActivePower ."
				+ " ?Pmin   j2:hasValue ?vPmin ."
				+ " ?vPmin   j2:numericalValue ?Pminval ."
				
				+ "?entity j9:hasActivePowerAbsorbed ?Pact ."
				+ "?Pact a j9:AbsorbedActivePower ."
				+ " ?Pact   j2:hasValue ?vPact ."
				+ " ?vPact   j2:numericalValue ?Pactval ."
				+ " ?vPact   j7:hasTime ?proptime ."
				+ "?proptime j7:hour ?hourval ."
			
				+ "}"
				+ "ORDER BY ASC(?hourval)";

		
		 //?user  ?user ?equipment

		
		ResultSet resultSet = JenaHelper.query(model, groupInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		System.out.println("sizeofresult="+resultList.size());
		int size=resultList.size();
		List<String> iriofgroupuser= new ArrayList<String>();
		List<String[]> csvofbcap= new ArrayList<String[]>();
		for(int d=0;d<size;d++) {
			for(int t=0;t<keys.length;t++) {
				//System.out.println("elementonquery1 "+t+"= "+resultList.get(d)[t]);
				if(t==3) {
					iriofgroupuser.add(resultList.get(d)[t]);
				}

			}
			String[]e= {resultList.get(d)[3],resultList.get(d)[2]};
			csvofbcap.add(e);
			
			//System.out.println("---------------------------------------");
		}
		Collections.sort(csvofbcap, new Comparator<String[]>() {
			public int compare(String[] strings, String[] otherStrings) {
				return strings[0].compareTo(otherStrings[0]);
			}
		});
		String bcapcsv = MatrixConverter.fromArraytoCsv(csvofbcap);
		System.out.println(bcapcsv);
		
		
		//part 2 to see how many multiplication factor
		ResultSet resultSet2 = JenaHelper.query(model, groupInfo2);
		String result2 = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultList2 = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		System.out.println("sizeofresult="+resultList2.size());
		int size2=resultList2.size();
		for(int d=0;d<size2;d++) {
			for(int t=0;t<keys2.length;t++) {
				System.out.println("elementonquery2 "+t+"= "+resultList2.get(d)[t]);
			}
			System.out.println("---------------------------------------");
			
		}
		
		
		

		int sizeofiriuser=iriofgroupuser.size();
		Collections.sort(iriofgroupuser);
		System.out.println("sizeofiriuser="+sizeofiriuser);
		List<String[]> csvofpmax= new ArrayList<String[]>();
		List<String[]> csvofpmin= new ArrayList<String[]>();
		List<String[]> csvofw= new ArrayList<String[]>();
		List<String[]> csvofschedule= new ArrayList<String[]>();
		List<String>header=new ArrayList<String>();
		header.add("");
		for(int x=1;x<=sizeofiriuser;x++) {
			OntModel model2 = readModelGreedyForUser(iriofgroupuser.get(x-1));
			ResultSet resultSetx = JenaHelper.query(model2, equipmentinfo);
			String resultx = JenaResultSetFormatter.convertToJSONW3CStandard(resultSetx);
			String[] keysx = JenaResultSetFormatter.getKeys(resultx);
			List<String[]> resultListx = JenaResultSetFormatter.convertToListofStringArrays(resultx, keysx);
			System.out.println("sizeofresult="+resultListx.size());

			List<String>groupPmax=new ArrayList<String>();
			groupPmax.add(iriofgroupuser.get(x-1));
			List<String>groupPmin=new ArrayList<String>();
			groupPmin.add(iriofgroupuser.get(x-1));
			List<String>groupw=new ArrayList<String>();
			groupw.add(iriofgroupuser.get(x-1));
			List<String>groupschedule=new ArrayList<String>();
			groupschedule.add(iriofgroupuser.get(x-1));
			//Set to ensure no repeats
			int countr = 1; 
			groupschedule.add("t1");
			for(int d=0;d<resultListx.size();d++) {
					if(resultListx.get(d)[5].contentEquals("1")) {
						//System.out.println("equipment= "+resultListx.get(d)[0]);
						if(x==1) {
						header.add(resultListx.get(d)[0].split("#")[1].split("-")[0]);
						}
						groupPmax.add(resultListx.get(d)[1]);
						groupPmin.add(resultListx.get(d)[2]);
						groupw.add(resultListx.get(d)[3]);
					}
					//HashMap
					countr ++; 
					if (countr < 12) { //11 appliances
						groupschedule.add(resultListx.get(d)[4]);
					} else {
						groupschedule.add(resultListx.get(d)[4]);
						String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
						csvofschedule.add(arr4);
						//clear groupschedule
						groupschedule=new ArrayList<String>();
						countr = 1;
						if (Integer.parseInt(resultListx.get(d)[5]) < 24) {
							groupschedule.add(iriofgroupuser.get(x-1));
							groupschedule.add("t"+Integer.toString(Integer.parseInt(resultListx.get(d)[5])+1));
						}
					}				
			}

			
			String[] arr1 = groupPmax.toArray(new String[groupPmax.size()]);
			csvofpmax.add(arr1);
			String[] arr2 = groupPmin.toArray(new String[groupPmin.size()]);
			csvofpmin.add(arr2);
			String[] arr3 = groupw.toArray(new String[groupw.size()]);
			csvofw.add(arr3);
			String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
			csvofschedule.add(arr4);

		}
		String[] arr0 = header.toArray(new String[header.size()]);		
		
		csvofpmax.add(0, arr0);
		String pmaxcsv = MatrixConverter.fromArraytoCsv(csvofpmax);
		System.out.println(pmaxcsv);

		csvofpmin.add(0, arr0);
		String pmincsv = MatrixConverter.fromArraytoCsv(csvofpmin);
		System.out.println(pmincsv);
		
		csvofw.add(0, arr0);
		String wcsv = MatrixConverter.fromArraytoCsv(csvofw);
		System.out.println(wcsv);
		
		//csvofschedule.add(0, arr0);
		String schedulecsv = MatrixConverter.fromArraytoCsv(csvofschedule);
		System.out.println(schedulecsv);
		
	}
	
	public void testquerygen() {
		OntModel model = readModelGreedy(ENIRI);
		List<String[]> producer = new DistributedEnergySystem().provideGenlist(model); // instance iri
		//List<String[]> consumer = new DistributedEnergySystem().provideLoadFClist(model); // instance iri
	}
	
	public void testCreateJSON() {
		String baseUrl="D:\\JPS-git\\JParkSimulator-git\\JPS_DES\\workingdir";
		JSONObject d= new DistributedEnergySystem().convertResultJSON(baseUrl);
		System.out.println(d.toString());
	}
	

	
	
}
