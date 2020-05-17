
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
		
	/** Call this to run reset (upload all files to repository)
	 * require cityiri, location
	 */
	public void testAgentCallreset() {
		JSONObject jo = new JSONObject();
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/resetAirQualityRepository", jo.toString());	
	}
	/** Call this to run periodic update
	 * require cityiri
	 */
	public void testAgentCallfrom() {
		JSONObject jo = new JSONObject();
		jo.put("cityiri","http://dbpedia.org/resource/Singapore" );
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/AirQualitySensorAgent", jo.toString());	
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
	public void testCallAPI() {
		AirQualitySensorAgent ag = new AirQualitySensorAgent();
		ArrayList<JSONObject> jo = ag.getDataFromAPI();
		System.out.println(jo.toString());
	}

}
