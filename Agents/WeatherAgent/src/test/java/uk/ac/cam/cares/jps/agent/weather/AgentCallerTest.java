package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;

import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONObject;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;


public class AgentCallerTest {

	@Test
	public void testAgentCall() {
		String url = "http://kg.cmclinnovations.com:81/weather-agent/UpdateStation";
		JSONObject request = new JSONObject();
		request.put("station", "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontostation/OntoStation.owl#weatherstation_c4456fd1-1a20-452a-9d8f-9a7724e33837");
		AgentCaller.executeGetWithURLAndJSON(url, request.toString());
	}
	
	@Test
	public void createNameSpace() throws ClientProtocolException, IOException {
		String postbody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"
	    		+ "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n"
	    		+ "com.bigdata.rdf.sail.isolatableIndices=false\r\n"
	    		+ "com.bigdata.namespace.weather.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n"
	    		+ "com.bigdata.rdf.sail.truthMaintenance=false\r\n"
	    		+ "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n"
	    		+ "com.bigdata.rdf.sail.namespace=weather\r\n"
	    		+ "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n"
	    		+ "com.bigdata.namespace.weather.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"
	    		+ "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=true\r\n"
	    		+ "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";
		
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost("http://localhost:8080");
		postRequest.setEntity(new StringEntity(postbody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
	}
}
