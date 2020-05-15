package uk.ac.cam.cares.jps.dispersion.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.json.JSONObject;

import com.google.gson.Gson;

import junit.framework.TestCase;
import kong.unirest.HttpResponse;
import kong.unirest.Unirest;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
//import uk.ac.cam.cares.jps.dispersion.sensor.Token;
import uk.ac.cam.cares.jps.dispersion.sensor.Token;

public class AirQualitySensorAgentTest extends TestCase {
	
//	public static HttpClient GetClient(String token){
//		var authValue = new AuthenticationHeaderValue("Bearer", token);
//		var client = new HttpClient(){
//			DefaultRequestHeaders = { 
//					Authorization = authValue}//Set some other client defaults like timeout / BaseAddress
//			};
//			return client;
//			}
	
	
	
	
	
	public void testAQMeshAPI() throws IOException {
		String url="https://api.aqmeshdata.net/api";
		String urltest="https://apitest.aqmeshdata.net/api";
        JSONObject jo= new JSONObject();
        jo.put("username", "Cares1");
        jo.put("password", "Cares1Pa55word#");

	        String resp=AgentCaller.executePost("https://api.aqmeshdata.net/api/Authenticate", jo.toString());
	        JSONObject respjo= new JSONObject(resp);
	        String key=respjo.getString("token");
	        //String secondcall=AgentCaller.executeGet("https://api.aqmeshdata.net/api/Pods/Assets");
	        //String secondcall=AgentCaller.executeGet("https://api.aqmeshdata.net/api/Pods/GasFrequencies");
	        
	     // Sending get request
//	        URL url2 = new URL("https://api.aqmeshdata.net/api/LocationData/Next/2450495/1/01/0");
	        URL url2 = new URL("https://api.aqmeshdata.net/api/Pods/Assets");
	        HttpURLConnection conn = (HttpURLConnection) url2.openConnection();

	        conn.setRequestProperty("Authorization","Bearer:"+key);
	        //e.g. bearer token= eyJhbGciOiXXXzUxMiJ9.eyJzdWIiOiPyc2hhcm1hQHBsdW1zbGljZS5jb206OjE6OjkwIiwiZXhwIjoxNTM3MzQyNTIxLCJpYXQiOjE1MzY3Mzc3MjF9.O33zP2l_0eDNfcqSQz29jUGJC-_THYsXllrmkFnk85dNRbAw66dyEKBP5dVcFUuNTA8zhA83kk3Y41_qZYx43T

	        conn.setRequestProperty("Content-Type","application/json");
	        conn.setRequestMethod("GET");


	        BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
	        String output;

	        StringBuffer response = new StringBuffer();
	        while ((output = in.readLine()) != null) {
	            response.append(output);
	        }

	        in.close();
	        // printing result from response
	        System.out.println("Response:-" + response.toString());

//	        String secondcall=AgentCaller.executeGet("https://api.aqmeshdata.net/api/LocationData/Next/2450495/1/01/0");
//	        System.out.println("response= "+secondcall);
		

	}
	
	public void testAQMEsh2() {
		//Get token information by password. Only valid for 120 min
		HttpResponse<String> responsetoken = Unirest.post("https://api.aqmeshdata.net/api/Authenticate")
				.header("Content-Type", "application/json")
				.body("{\"username\":\"Cares1\",\"password\":\"Cares1Pa55word#\"}\r\n").asString();
		System.out.println(responsetoken.getBody());

		//Modify the token string. I'm not using the best the method...
		Gson g = new Gson();
		Token p = g.fromJson(responsetoken.getBody(), Token.class);
		String currenttoken = ("Bearer " + p.gettoken());
//		System.out.println(currenttoken);

		//Get pod information using the token
		HttpResponse<String> responsepod = Unirest.get("https://api.aqmeshdata.net/api/Pods/Assets")
				.header("Authorization", currenttoken).asString();
		System.out.println(responsepod.getBody());
		
		//Get Gas and temperature measurement data using the token
				HttpResponse<String> responseGas = Unirest.get("https://api.aqmeshdata.net/api/LocationData/Next/1740/1/01")
					      .header("Accept", "application/json")
					      .header("Authorization", currenttoken).asString();
		System.out.println(responseGas.getBody());
		
		//Get PM measurement data using the token
		HttpResponse<String> responsePM = Unirest.get("https://api.aqmeshdata.net/api/LocationData/Next/1740/2/01/1")
			      .header("Accept", "application/json")
			      .header("Authorization", currenttoken).asString();
		System.out.println(responsePM.getBody());
	}
	

	
	public void testtimeformat() throws ParseException {
		String date="16/Apr/2020 12:00:00";
		//Date date1=new SimpleDateFormat("yyyy-mmm-dd hh:mm:ss").parse(date);
		SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MMM/yyyy HH:mm:ss");
		
		Date date1=dateFormat.parse(date);  
		String timeformatted=new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").format(date1)+"+08:00";
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
