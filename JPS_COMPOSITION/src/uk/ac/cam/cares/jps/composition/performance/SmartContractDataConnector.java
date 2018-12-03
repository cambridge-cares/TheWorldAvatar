package uk.ac.cam.cares.jps.composition.performance;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class SmartContractDataConnector {

	public static String myHost = "www.theworldavatar.com";
	public static int myPort = 85;
	
	public static Map<String, Long[]> findScoresForAgents() throws JSONException {
		// 1. get all agents
		// 2. get all the iris of the agents, find the relevant ones. 

		
		URIBuilder get_all_agents = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/get_all_agents");
		JSONArray agentAddresses = new JSONArray(executeGet(get_all_agents));
		Map<String, Long[]> iriToScores = new HashMap<String, Long[]>();
		
		for(int i = 0; i < agentAddresses.length(); i++) {
			String agentAddress =  agentAddresses.getString(i);
			URIBuilder get_iri = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
					.setPath("/get_agent_iri")
					.setParameter("agent_id",agentAddress);
			String _iri = executeGet(get_iri);
			Long[] score_array =  getScoreForOneAgent(agentAddress);
			System.out.println("============ score array ==============");
			System.out.println(score_array[0] + "|" + score_array[1]);
			System.out.println("=======================================");
			iriToScores.put(_iri, score_array);
		}
		
		return iriToScores;
		
	}
	
	public static Long[] getScoreForOneAgent(String agentAddress) throws JSONException {

		System.out.println("======== agent address ========");
		System.out.println(agentAddress);
		URIBuilder get_score = new URIBuilder().setScheme("http").
				setHost(myHost).setPort(myPort)
				.setPath("/get_score")
				.setParameter("agent_id", agentAddress);
		JSONArray scores = new JSONArray(executeGet(get_score));
		System.out.println("scores: " + scores);
		System.out.println("===============================");

		return new Long[] {scores.getLong(0), scores.getLong(1)};
	
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
	
	
}
