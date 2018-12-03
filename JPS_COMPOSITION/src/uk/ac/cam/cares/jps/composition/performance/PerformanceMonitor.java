package uk.ac.cam.cares.jps.composition.performance;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class PerformanceMonitor {

	public static String[] agents = new String[]{"/JPS_COMPOSITION/CityToWeather","/JPS_COMPOSITION/MockCityToWeather_Accu","/JPS_COMPOSITION/MockCityToWeather_Yahoo"};
	public static Map<String, String> id_map = new HashMap();
	public static Map<String, String> iri_map = new HashMap();
	public static Map<String, String> price_map = new HashMap();
	
	public static long day = 80640000;

	public static String myHost = "localhost";
	public static int myPort = 88;
	public static void main(String [] args) throws JSONException, InterruptedException
	{
		
		id_map.put("/JPS_COMPOSITION/CityToWeather", "0x0B9056fcbf59D283F7c6B909Ea729182Bd69D36E");
		id_map.put("/JPS_COMPOSITION/MockCityToWeather_Accu", "0x700B09517ddaCF44617371D8D00bB09FE9425FC8");
		id_map.put("/JPS_COMPOSITION/MockCityToWeather_Yahoo", "0x1c5048f718289e8b7050648ceb9b330a7b3f310f");
	
		iri_map.put("/JPS_COMPOSITION/CityToWeather","http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service");
		iri_map.put("/JPS_COMPOSITION/MockCityToWeather_Accu","http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service");
		iri_map.put("/JPS_COMPOSITION/MockCityToWeather_Yahoo","http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service");
		
		price_map.put("0x0B9056fcbf59D283F7c6B909Ea729182Bd69D36E", Long.toString(50000000 * 100000000));
		price_map.put("0x700B09517ddaCF44617371D8D00bB09FE9425FC8",  Long.toString(30000000 * 100000000));
		price_map.put("0x1c5048f718289e8b7050648ceb9b330a7b3f310f", Long.toString(30000000 * 100000000));
 
		for(String agent_path: agents) {
			System.out.println("Register");
			String agent_id = id_map.get(agent_path);
			String agent_iri = iri_map.get(agent_path);
			register_agents(agent_id, agent_iri);
		}
		
		
		for(int i = 0; i< 100; i++) {
			for(String cityIRI : getCityList().subList(0, 50)) {
				for(String path : agents) {
				//String path = agents[0];
					System.out.println("Calling : " + path);
					MakeCallAndShowParameters(path,cityIRI);
				}
			}
		}
	}

	
	public static void register_agents(String agent_id,String agent_iri) {

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/register_agent")
				.setParameter("agent_id", agent_id)
				.setParameter("agent_iri", agent_iri)
				.setParameter("price", price_map.get(agent_id))
				.setParameter("token", "85564251");	
		String result = executeGet(builder);	
		System.out.println("============= result of registering =============");
		System.out.println(result);
		System.out.println("=================================================");
	}
	
	public static String get_token_for_agent(String agent_id) {
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/get_token_for_agent")
				.setParameter("agent_id", agent_id)
				.setParameter("client_address", "0xbaD9E32129bBC6025D50E433D2d45167B9f4774e")
				.setParameter("client_private_key", "85564251")				
				;
		String result = executeGet(builder);
		return result;
	}
	
	
	public static String verify_token_for_agent(String agent_id, String time_stamp,String private_key) {

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/verify_token_for_agent")
				.setParameter("client_address", "0xbaD9E32129bBC6025D50E433D2d45167B9f4774e")
				.setParameter("agent_id", agent_id)
				.setParameter("time_stamp", time_stamp)				
				.setParameter("private_key", "85564251")				
				;
		//address client_address, address agent_address, uint time_stamp, uint private_key_of_agent
		String result = executeGet(builder);
		return result;
	}
	
	public static String pay_deposit() {

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/pay_deposit")
				.setParameter("client_private_key", "85564251");
		return executeGet(builder);
	}
	
	public static void start() {
		

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/start");
		String result = executeGet(builder);
	}
	
	public static void MakeCallAndShowParameters(String path, String cityIRI) throws JSONException, InterruptedException {
		JSONObject res = callAgent(path,cityIRI);
		long time = res.getLong("time");
		int code = res.getInt("code");
		long timeStamp = res.getLong("time_stamp");
		String result = res.getString("result");
		double coverage = checkCoverage(result);
		
		//updateAScoreMatrix(long time, int code, double coverage, String agent_id)
		updateAScoreMatrix(time, code, coverage, timeStamp, id_map.get(path));
	}
	
	
	public static void Make_Payment() {
		String agent_id = "0x0B9056fcbf59D283F7c6B909Ea729182Bd69D36E";
	
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/make_payment")
				.setParameter("agent_id", agent_id);
		executeGet(builder);
	}
	
	public static JSONObject callAgent(String path, String cityIRI) throws JSONException {
		
		long startTime = System.currentTimeMillis();
		String result = makeARequest(path,cityIRI);	
		
		Random rand = new Random();
		int n = rand.nextInt(30) - 30;
		long timeOffSet = n * day;
		long timeStamp = System.currentTimeMillis() + timeOffSet;
		long endTime = System.currentTimeMillis();
		JSONObject o = new JSONObject();
		o.put("time", endTime - startTime);
		o.put("time_stamp", timeStamp);
		if(result.equalsIgnoreCase("")) {
			o.put("code", 404);
		}
		else {
			o.put("code", 200);
		}
		o.put("result", result);
		return o;
	}
	
	public static void updateAScoreMatrix(long time, int code, double coverage, long timeStamp, String agent_id) throws JSONException {
		JSONArray scoreMatrix = new JSONArray();
		scoreMatrix.put(time);
		scoreMatrix.put(code);
		scoreMatrix.put(coverage);
		scoreMatrix.put(timeStamp);
		

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/update_score")
				.setParameter("score_matrix", scoreMatrix.toString())
				.setParameter("agent_id", agent_id);	
		String result = executeGet(builder);	 
		System.out.println("Result from updating score: " + result);
	}
	
	public static String makeARequest(String path,String cityIRI) throws JSONException {
	
		JSONObject input = new JSONObject();
		input.put("city", cityIRI);

		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(path)
				.setParameter("query", input.toString());				;
		String result = executeGet(builder);	 
		return result;
	}
	
	public static double checkCoverage(String resultString) {
		int occurance = StringUtils.countMatches(resultString, "null");
		System.out.println("Occurance: " + occurance);
		if(occurance == 0) {
			return 1.0;
		}
		else {
			return (double)(7 - occurance)/7;
		}
	}
	
	
	public static String executeGet(URIBuilder builder) { // TODO: ZXC: Put this function in utility
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		} 
	}
	
	public static ArrayList<String> getCityList() throws JSONException {
		
		ArrayList<String> result = new ArrayList<String>();
		JSONArray a = new JSONArray("["+
				"		\"http://dbpedia.org/resource/The_Hague\",\r\n" + 
				"		\"http://dbpedia.org/resource/Berlin\",\r\n" + 
				"		\"http://dbpedia.org/resource/Acre,_Israel\",\r\n" + 
				"		\"http://dbpedia.org/resource/AntiochXX\",\r\n" + 
				"		\"http://dbpedia.org/resource/Ashkelon\",\r\n" + 
				"		\"http://dbpedia.org/resource/Balanjar\",\r\n" + 
				"		\"http://dbpedia.org/resource/Ctesiphon\",\r\n" + 
				"		\"http://dbpedia.org/resource/Espoo\",\r\n" + 
				"		\"http://dbpedia.org/resource/Gaza_City\",\r\n" + 
				"		\"http://dbpedia.org/resource/Haifa\",\r\n" + 
				"		\"http://dbpedia.org/resource/Hebron\",\r\n" + 
				"		\"http://dbpedia.org/resource/Independent_city\",\r\n" + 
				"		\"http://dbpedia.org/resource/Jenin\",\r\n" + 
				"		\"http://dbpedia.org/resource/Khazaran\",\r\n" + 
				"		\"http://dbpedia.org/resource/Leptis_Magna\",\r\n" + 
				"		\"http://dbpedia.org/resource/Lord_Mayor_of_London\",\r\n" + 
				"		\"http://dbpedia.org/resource/Petra\",\r\n" + 
				"		\"http://dbpedia.org/resource/Porvoo\",\r\n" + 
				"		\"http://dbpedia.org/resource/Ramallah\",\r\n" + 
				"		\"http://dbpedia.org/resource/Samandar_(city)\",\r\n" + 
				"		\"http://dbpedia.org/resource/Sardis\",\r\n" + 
				"		\"http://dbpedia.org/resource/Segesta\",\r\n" + 
				"		\"http://dbpedia.org/resource/Selinunte\",\r\n" + 
				"		\"http://dbpedia.org/resource/Sevastopol\",\r\n" + 
				"		\"http://dbpedia.org/resource/Troy\",\r\n" + 
				"		\"http://dbpedia.org/resource/Turku\",\r\n" + 
				"		\"http://dbpedia.org/resource/Urban_heat_island\",\r\n" + 
				"		\"http://dbpedia.org/resource/Vantaa\",\r\n" + 
				"		\"http://dbpedia.org/resource/West_Berlin\",\r\n" + 
				"		\"http://dbpedia.org/resource/Aksu,_Kazakhstan\",\r\n" + 
				"		\"http://dbpedia.org/resource/Cebu_City\",\r\n" + 
				"		\"http://dbpedia.org/resource/Chagai,_Pakistan\",\r\n" + 
				"		\"http://dbpedia.org/resource/Mud,_Iran\",\r\n" + 
				"		\"http://dbpedia.org/resource/Nandgaon,_Maharashtra\",\r\n" + 
				"		\"http://dbpedia.org/resource/Nandgaon,_Uttar_Pradesh\",\r\n" + 
				"		\"http://dbpedia.org/resource/Padmavati_Pawaya\",\r\n" + 
				"		\"http://dbpedia.org/resource/Raman,_Punjab_(India)\",\r\n" + 
				"		\"http://dbpedia.org/resource/Reti,_Pakistan\",\r\n" + 
				"		\"http://dbpedia.org/resource/Siguatepeque\",\r\n" + 
				"		\"http://dbpedia.org/resource/Tamazula_de_Victoria\",\r\n" + 
				"		\"http://dbpedia.org/resource/Zamboanga_City\",\r\n" + 
				"		\"http://dbpedia.org/resource/'Amran\",\r\n" + 
				"		\"http://dbpedia.org/resource/Aboh\",\r\n" + 
				"		\"http://dbpedia.org/resource/Akbarabad,_Sarduiyeh\",\r\n" + 
				"		\"http://dbpedia.org/resource/Akwa_Akpa\",\r\n" + 
				"		\"http://dbpedia.org/resource/Al-Rastan\",\r\n" + 
				"		\"http://dbpedia.org/resource/Angola_(Book_of_Mormon)\",\r\n" + 
				"		\"http://dbpedia.org/resource/Antigonia_(Syria)\",\r\n" + 
				"		\"http://dbpedia.org/resource/Antigonia_Psaphara\",\r\n" + 
				"		\"http://dbpedia.org/resource/Arauco,_Chile\",\r\n" + 
				"		\"http://dbpedia.org/resource/Badi,_Raisen\",\r\n" + 
				"		\"http://dbpedia.org/resource/Bageis\"]");
		
		for(int i = 0; i < a.length(); i++) {
			String cityIRI = a.getString(i);
			result.add(cityIRI.trim());
		}
		
		return result;

	}
	
	
}
