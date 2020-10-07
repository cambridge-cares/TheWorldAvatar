package uk.ac.cam.cares.des.test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthGetBalance;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.core.methods.response.Web3ClientVersion;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Transfer;
import org.web3j.utils.Convert;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.des.BlockchainWrapper;
import uk.ac.cam.cares.jps.des.DistributedEnergySystem;
import uk.ac.cam.cares.jps.des.FrontEndCoordination;
import uk.ac.cam.cares.jps.des.WeatherIrradiationRetriever;

public class Test_DES extends TestCase{
	
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/SingaporeElectricalNetwork.owl#SingaporeElectricalNetwork";
	private String DISIRI="http://www.theworldavatar.com/kb/sgp/singapore/District-001.owl#District-001";
	
	/*
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

	public void testStartDESAgent() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		jo.put("district", DISIRI);
		jo.put("temperaturesensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
    	jo.put("irradiationsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
    	jo.put("windspeedsensor","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	/*
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
	/*
	 * Calls and runs the Blockchain transaction with test values
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
	public String getLastModifiedDirectory() {
    	String agentiri = "http://www.theworldavatar.com/kb/agents/Service__DESAgent.owl#Service";
		List<String> lst = null;
    	System.out.println(lst);
    	String resultfromfuseki = MetaDataQuery.queryResources(null,null,null,agentiri, null, null,null,lst);
		 String[] keys = JenaResultSetFormatter.getKeys(resultfromfuseki);
		 List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromfuseki, keys);
    	return listmap.get(0)[0];
    }
	/*
	 * Calls and runs the Blockchain transaction with test values (thru TOMCAT)
	 */
	public void testBlockchainWrapperAgentCall() throws IOException{
		JSONObject jo = new JSONObject();
		jo.put("industrial", "2.311116263469459966e+01");
		jo.put("commercial", "5.000000000000000000e+01");
		jo.put("residential","8.826121920185781278e+00");
		jo.put("gridsupply","4.409266691007480290e+01");
		jo.put("solar","3.784461764480557235e+01");
		jo.put("directory",getLastModifiedDirectory());
	    System.out.println(jo.toString());
		String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", jo.toString());
		System.out.println(v);
	}
	

	/*
	 * Calls and runs the hourly weather retriever, that uses OCR
	 */
	public void testIrradiationRetreiverDirectCall() throws Exception {
//		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = "C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\cbf06a1c-5046-4708-a5d6-aaa696856e54\\JPS_DES";
		
		JSONObject jo = new JSONObject();
		
		jo.put("folder", baseUrl);
		jo.put("tempsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001");
		jo.put("speedsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
		jo.put("irradiationsensor", "http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001");
		jo.put("jpscontext", "base");
		WeatherIrradiationRetriever a= new WeatherIrradiationRetriever();

		a.readWritedatatoOWL(baseUrl,"http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl#SGTemperatureSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001","http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedSensor-001.owl#SGWindSpeedSensor-001");
	}
	/*
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

	/*
	 * Tests the retrieval of data from one sensor. 
	 */
	public void testcsvmanipulation () {
		 String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
					+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
					+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" +"ORDER BY ASC(?proptimeval)";
		
		String iriirradiationsensor="http://localhost:8080/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		String result2 = new QueryBroker().queryFile(iriirradiationsensor, sensorinfo);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		System.out.println("sizeofresult="+resultListfromqueryirr.size());
		System.out.println("element= "+resultListfromqueryirr.get(0)[2]);
		String content=resultListfromqueryirr.get(23)[2];
		System.out.println("content="+content);
		System.out.println("year= "+content.split("-")[0]);
		System.out.println("month= "+content.split("-")[1]);
		System.out.println("date= "+content.split("-")[2].split("T")[0]);
		System.out.println("time= "+content.split("-")[2].split("T")[1].split("\\+")[0]);
	}
	/** test retrieval from a forecast
	 * 
	 * @param iriofnetwork
	 * @return
	 */
	public void testcsvmanipulation2 () {
		String irioftempF= "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
	        String iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
	        String iriofwindF="http://www.theworldavatar.com/kb/sgp/singapore/SGWindSpeedForecast-001.owl#SGWindSpeedForecast-001";

	        String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:T-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" ;

	
		String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" ;

	
		String sensorinfo3 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
				+ "WHERE { ?entity a j5:F-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
				+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}";

		
		//grab forecast results
		String result = new QueryBroker().queryFile(irioftempF, sensorinfo+ "ORDER BY ASC(?proptimeval)");
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquerytemp = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		System.out.println("Temperature " + resultListfromquerytemp.toString());
		String result2 = new QueryBroker().queryFile(iriofirrF, sensorinfo2+ "ORDER BY ASC(?proptimeval)");
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		System.out.println("Irradiation " + resultListfromqueryirr.toString());
		String result3 = new QueryBroker().queryFile(iriofwindF, sensorinfo3 + "ORDER BY ASC(?proptimeval)");
		String[] keys3 = JenaResultSetFormatter.getKeys(result3);
		List<String[]> resultListfromqueryspeed = JenaResultSetFormatter.convertToListofStringArrays(result3, keys3);
		System.out.println("Speed " +resultListfromqueryspeed);
		ArrayList<String[]> readingFromCSV = new ArrayList<String[]>();
		
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
		String[] timeschedu = {"t1","t2", "t3", "t4", "t5","t6","t7", "t8", "t9", "t10","t11","t12", "t13", "t14", "t15","t16","t17", "t18", "t19", "t20","t21","t22", "t23", "t24"};
		//grab the current time
		List<String> lst = Arrays.asList(timeschedu);
		Date date = new Date();   // given date
		Calendar calendar = GregorianCalendar.getInstance(); // creates a new calendar instance
		calendar.setTime(date);   // assigns calendar to given date 
		int h = calendar.get(Calendar.HOUR_OF_DAY); // gets hour in 24h format
		
		//rotate it according to the current hour to get the appropriate profile
		Collections.rotate(lst, h);
		header.add("");
		for(int x=1;x<=sizeofiriuser;x++) {
			List<String[]> subList =  new ArrayList<String[]>();
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
			groupschedule.add(lst.get(0));
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
						subList.add(arr4);
						//clear groupschedule
						groupschedule=new ArrayList<String>();
						countr = 1;
						if (Integer.parseInt(resultListx.get(d)[5]) < 24) {
							groupschedule.add(iriofgroupuser.get(x-1));
//							groupschedule.add("t"+Integer.toString(Integer.parseInt(resultListx.get(d)[5])+1));
							groupschedule.add(lst.get(Integer.parseInt(resultListx.get(d)[5])));
						}
					}				
			}

			
			String[] arr1 = groupPmax.toArray(new String[groupPmax.size()]);
			csvofpmax.add(arr1);
			String[] arr2 = groupPmin.toArray(new String[groupPmin.size()]);
			csvofpmin.add(arr2);
			String[] arr3 = groupw.toArray(new String[groupw.size()]);
			csvofw.add(arr3);
			System.out.println(groupschedule.toArray().toString());
			String[] arr4 = groupschedule.toArray(new String[groupschedule.size()]);
			subList.add(arr4);
			Collections.rotate(subList, -h);
			csvofschedule.addAll(subList);

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
		
		csvofschedule.add(0, arr0);
		String schedulecsv = MatrixConverter.fromArraytoCsv(csvofschedule);
		System.out.println(schedulecsv);
		
	}
	/*
	 * tests the call of electrical network
	 */
	public void testquerygen() {
		OntModel model = readModelGreedy(ENIRI);
		List<String[]> producer = new DistributedEnergySystem().provideGenlist(model); // instance iri
		//List<String[]> consumer = new DistributedEnergySystem().provideLoadFClist(model); // instance iri
	}
	
	public void testCreateJSON() {
		String baseUrl="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data\\3d64a110-ce59-4f9b-97a6-e2a80eb3c7f7\\JPS_DES";
		JSONObject d= new DistributedEnergySystem().provideJSONResult(baseUrl);
		System.out.println(d.toString());
	}
	/*
	 * Finds the latest directory, as part of the coordinate agent. 
	 */
	public void testfindlatestdirectory() {
		 String dir="C:\\JPS_DATA\\workingdir\\JPS_SCENARIO\\scenario\\base\\localhost_8080\\data";
		 File baseUrl=new File(dir);
		System.out.println("date latest directory= "+ new FrontEndCoordination().getLastModifiedDirectory());
	}
	public static void testBlockchainInteraction() throws IOException {
		Web3j web3 = Web3j.build(new HttpService("https://rinkeby.infura.io/v3/1f23f6038dde496ea158547e3ba1e76b"));
		Web3ClientVersion web3ClientVersion = web3.web3ClientVersion().send();
		String clientVersion = web3ClientVersion.getWeb3ClientVersion();
		System.out.println("Connected to Ethereum Client Version: " + clientVersion);
		try {
			//Get balance
			EthGetBalance ethGetBalance=web3.ethGetBalance("0x1eD35d5845F8162B40df26c34562cFabd4892017", DefaultBlockParameterName.LATEST).sendAsync().get();
			java.math.BigInteger wei = ethGetBalance.getBalance();
			System.out.println(wei);
			Credentials credentials = WalletUtils.loadCredentials("Caesar1!", "C:\\Users\\LONG01\\TOMCAT\\webapps\\JPS_DES##1.0.0\\resources\\residential.json");
			TransactionReceipt transactionReceipt = Transfer.sendFunds(
			        web3, credentials, "0x9e64A50EfA603BCD127001b689635fca4669ba9d",
			        BigDecimal.valueOf(1.0), Convert.Unit.ETHER).send();
			System.out.println(transactionReceipt);
//			String pk = "04dab771c776d8345c8877a70f26c03a3bd7927abbc65ceff14e74ee23ab0fe8"; //private key of industrial
//		   Credentials credentials = Credentials.create(pk);
		}catch (Exception ex) {
			System.out.println(ex);
		}
	
	}
	
	

	
	
}
