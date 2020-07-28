package uk.ac.cam.cares.jps.dispersion.test;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.json.JSONObject;

import junit.framework.TestCase;
import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.dispersion.sensor.AirQualitySensorAgent;

public class AirQualitySensorAgentTest extends TestCase {
	
//	public static HttpClient GetClient(String token){
//		var authValue = new AuthenticationHeaderValue("Bearer", token);
//		var client = new HttpClient(){
//			DefaultRequestHeaders = { 
//					Authorization = authValue}//Set some other client defaults like timeout / BaseAddress
//			};
//			return client;
//			}
	
	/** Call this to run periodic update
	 * require cityiri
	 */
	public void testAgentCallfrom() {
		JSONObject jo = new JSONObject();
		jo.put("cityiri","http://dbpedia.org/resource/Singapore" );
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/AirQualitySensorAgent", jo.toString());	
	}	
	
	/** test that call toAPI is available
	 * 
	 */
	public void testAPIClear() {
		HttpResponse<String> response = Unirest.post("https://api.aqmeshdata.net/api/Authenticate")
				.header("Content-Type", "application/json")
				.body("{\"username\":\"Cares1\",\"password\":\"Cares1Pa55word#\"}\r\n").asString();
		String tokenPhrase = response.getBody();

	    assertNotNull(tokenPhrase);
	    assertEquals(200, response.getStatus());
	}
	/** test to see if response is created and what is the response like
	 * @throws Exception 
	 * 
	 */
	public void xxxtestCallAPI() throws Exception {
		AirQualitySensorAgent ag = new AirQualitySensorAgent();
		ArrayList<JSONObject> jo = ag.getDataFromAPI();
		System.out.println(jo.toString());
	}
	
	public void xxxtestdirect()  throws Exception {
		new AirQualitySensorAgent().executePeriodicUpdate("http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-001.owl#AirQualityStationAQMesh-001");
	}
	
	
	
	/** Call this to run reset (upload all files to repository)
	 * require cityiri, location
	 */
	public void xxxtestAgentCallreset() {
		JSONObject jo = new JSONObject();
		jo.put("location", "hongkong");//or singapore or singapore_AQ
		jo.put("context", "http://www.theworldavatar.com/kb/hkg/hongkong/AirQualityStation-002.owl#AirQualityStation-002");
		//context variation only in index number and country (1 sg,2 hk)
		jo.put("name","VirtualSensorHKEpisode-001");// or ="VirtualSensorEpisode-001"or=VirtualSensor-001 or="VirtualSensorHKADMS-001";
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/resetAirQualityRepository", jo.toString());	
	}
	

	
	public void testtimeformat() throws ParseException {
		String date="16/Apr/2020 12:00:00";
		//Date date1=new SimpleDateFormat("yyyy-mmm-dd hh:mm:ss").parse(date);
		SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MMM/yyyy HH:mm:ss");
		 DateFormat pstFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
		//pstFormat.setTimeZone(TimeZone.getTimeZone("GMT+8"));
		Date date1=dateFormat.parse(date);  
		String timeformatted=pstFormat.format(date1);
		System.out.println("new format= "+timeformatted);
	}
	
	public void testtimeformat2() throws ParseException {
		String metadataResult;
		long millis = System.currentTimeMillis();
		String toSimulationTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis);
		String fromSimulationTime = MetaDataAnnotator.getTimeInXsdTimeStampFormat(millis-3600*1000);
		System.out.println(toSimulationTime);
		System.out.println(fromSimulationTime);
	}
	
	public void testprovideNextTime() {			//timing format should be year, month, date, hour, minute,second

//		DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
//		   LocalDateTime now = LocalDateTime.now();
		   String com="2020-05-11T07:04:01.221";
		   
		   DateFormat utcFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
		   utcFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
		try {
			  Date date = utcFormat.parse(com);
			   DateFormat pstFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
			   pstFormat.setTimeZone(TimeZone.getTimeZone("GMT+8"));

			   System.out.println(pstFormat.format(date));
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}


			

	}
	

	public void testmakecsv() {
//		String sensorinfo="PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + 
//				"				 PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> \r\n" + 
//				"				 PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> \r\n" + 
//				"				 PREFIX j6:<http://www.w3.org/2006/time#>  \r\n" + 
//				"				 PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> \r\n" + 
//				"				 SELECT Distinct ?prop ?propval ?unit  ?proptimeval ?allpsi ?mean ?max ?min ?individualpsi ?xval ?yval\r\n" + 
//				"				 {graph <http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-001.owl#AirQualityStationAQMesh-001>\r\n" + 
//				"				 { \r\n" + 
//				"          ?entity   j7:hasGISCoordinateSystem ?coordsys .\r\n" + 
//				"               ?coordsys   j7:hasProjectedCoordinate_x ?xent .\r\n" + 
//				"                ?xent j2:hasValue ?vxent .\r\n" + 
//				"               ?vxent   j2:numericalValue ?xval .\r\n" + 
//				"                ?coordsys   j7:hasProjectedCoordinate_y ?yent .\r\n" + 
//				"                ?yent j2:hasValue ?vyent .\r\n" + 
//				"                ?vyent   j2:numericalValue ?yval .\r\n" + 
//				"    ?graph j4:hasOverallPSI ?allpsi .\r\n" + 
//				"				  #?prop a j4:OutsideCOConcentration .\r\n" + 
//				"				  ?prop   j2:hasValue ?vprop . \r\n" + 
//				"    ?prop j4:hasMeasuredPropertyMean ?mean .\r\n" + 
//				"    ?prop j4:hasMeasuredPropertyMax ?max .\r\n" + 
//				"    ?prop j4:hasMeasuredPropertyMin ?min .\r\n" + 
//				"    ?prop j4:hasPSI ?individualpsi .\r\n" + 
//				"				?vprop   j4:prescaledNumValue ?propval . \r\n" + 
//				"    ?vprop   j2:hasUnitOfMeasure ?unit . \r\n" + 
//				"				  ?vprop   j6:hasTime ?proptime .\r\n" + 
//				"				  ?proptime   j6:inXSDDateTimeStamp ?proptimeval .\r\n" + 
//				"\r\n" + 
//				"				 } \r\n" + 
//				"				 } \r\n" + 
//				"				 ORDER BY DESC(?proptimeval)LIMIT10";
		
//		String sensorinfo = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
//				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
//				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
//				+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
//				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
//				+ "SELECT ?vprop ?proptime "

//				+ "{graph "+"<http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-001.owl#AirQualityStationAQMesh-001>"
//				+ "{ "
//				+ " ?prop a j4:OutsideSO2Concentration ."
//				+ " ?prop   j2:hasValue ?vprop ." 
//				+ " ?vprop   j6:hasTime ?proptime ."
//				+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ."
//				+ "}" 
//				+ "}" 
//				+ "ORDER BY ASC(?proptimeval)LIMIT1";
		
		String sensorinfo="PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				" PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#>\r\n" + 
				" PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#>\r\n" + 
				" PREFIX j6:<http://www.w3.org/2006/time#>\r\n" + 
				" PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				" SELECT Distinct ?vprop ?propval  ?proptimeval ?allpsi ?mean ?max ?min ?individualpsi \r\n" + 
				" {graph <http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-001.owl#AirQualityStationAQMesh-001> \r\n" + 
				" {\r\n" + 
				"  ?graph j4:hasOverallPSI ?allpsi .\r\n" + 
				" ?prop   j2:hasValue ?vprop .\r\n" + 
				"    ?prop j4:hasMeasuredPropertyMean ?mean .\r\n" + 
				"    ?prop j4:hasMeasuredPropertyMax ?max .\r\n" + 
				"    ?prop j4:hasMeasuredPropertyMin ?min .\r\n" + 
				"    ?prop j4:hasPSI ?individualpsi .\r\n" + 
				"?vprop   j4:prescaledNumValue ?propval .\r\n" + 
				"  ?vprop   j6:hasTime ?proptime .\r\n" + 
				"  ?proptime   j6:inXSDDateTimeStamp ?proptimeval .\r\n" + 
				"}}\r\n" + 
				" ORDER BY DESC(?proptimeval) LIMIT30";
		
		//in virtual sensor is 10 pollutant ,so limit=240
		//in aqmesh 9 pollutant = 9*12*24 -> should be 10 pollutant (co2 is missing) 10*12*24=2880
		String dataPath= QueryBroker.getLocalDataPath();
		Object[] a = KnowledgeBaseClient.createRequestUrl("http://www.theworldavatar.com/jps/data/airquality", null, true);
		String requestUrl = "http://www.theworldavatar.com/jps/data/airquality";
		System.out.println(requestUrl);
		JSONObject joparams = (JSONObject) a[1];
		if (joparams == null) {
			joparams = new JSONObject();
		}
		joparams.put(JPSConstants.QUERY_SPARQL_QUERY, sensorinfo);
		String resultfromrdf4j=Http.execute(Http.get(requestUrl, null, joparams));

		//String resultfromrdf4j = KnowledgeBaseClient.query("http://localhost:8080/jps/data/airquality", null, sensorinfo);
		String[] keys = JenaResultSetFormatter.getKeys(resultfromrdf4j);
		List<String[]> listmap = JenaResultSetFormatter.convertToListofStringArrays(resultfromrdf4j, keys);
		listmap.add(0,keys);
		System.out.println("res= "+MatrixConverter.fromArraytoCsv(listmap));
		//new QueryBroker().putLocal(dataPath + "/VirtualSensor.csv", MatrixConverter.fromArraytoCsv(listmap));
		
	}
}
