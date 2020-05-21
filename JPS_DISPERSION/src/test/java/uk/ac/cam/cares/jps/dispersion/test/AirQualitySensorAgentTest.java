package uk.ac.cam.cares.jps.dispersion.test;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;

import org.json.JSONObject;

import junit.framework.TestCase;
import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
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
	 * 
	 */
	public void xxxtestCallAPI() {
		AirQualitySensorAgent ag = new AirQualitySensorAgent();
		ArrayList<JSONObject> jo = ag.getDataFromAPI();
		System.out.println(jo.toString());
	}
	
	public void xxxtestdirect() {
		new AirQualitySensorAgent().executePeriodicUpdate("http://www.theworldavatar.com/kb/sgp/singapore/AirQualityStationAQMesh-001.owl#AirQualityStationAQMesh-001");
	}
	
	
	
	/** Call this to run reset (upload all files to repository)
	 * require cityiri, location
	 */
	public void xxxtestAgentCallreset() {
		JSONObject jo = new JSONObject();
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/resetAirQualityRepository", jo.toString());	
	}
	

	
	public void testtimeformat() throws ParseException {
		String date="16/Apr/2020 12:00:00";
		//Date date1=new SimpleDateFormat("yyyy-mmm-dd hh:mm:ss").parse(date);
		SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MMM/yyyy HH:mm:ss");
		 DateFormat pstFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
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
	
	public void xxxtestresetAirQualityClaudius() {
		JSONObject empty= new JSONObject();
		String resp=AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/resetAirQualityRepository", empty.toString());
	}


}
