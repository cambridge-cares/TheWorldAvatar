package uk.ac.cam.cares.jps.accessagent.integrationtest;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.http.Http;

/**
 * Contains methods for Access Agent integration tests
 * @author csl37
 */
public class IntegrationTestHelper {

	public static String uploadRoutingData(String label, String endpoint, String uploadUrl) {
		
		String routingData = getRoutingData(label, endpoint);
				
		HttpPost request = Http.post(uploadUrl, routingData, MediaType.APPLICATION_JSON.type, null);
		String result = Http.execute(request);	
		
		return new JSONObject(result).getString("result");
	}

	public static String getRoutingData(String label, String endpoint) {
		return "[\n"+
					"{\n"+
					"	\"label\": \""+label+"\",\n"+
					"	\"queryEndpoint\": \""+endpoint+"\",\n"+
					"	\"updateEndpoint\": \""+endpoint+"\"\n"+
					"}\n"+
				"]";
	}

	public static String uploadRDBRoutingData(String label, String url, String uploadUrl) {

		String routingData = getRDBRoutingData(label, url);

		HttpPost request = Http.post(uploadUrl, routingData, MediaType.APPLICATION_JSON.type, null);
		String result = Http.execute(request);

		return new JSONObject(result).getString("result");
	}

	public static String getRDBRoutingData(String label, String url) {
		return "[\n"+
				"{\n"+
				"	\"label\": \""+label+"\",\n"+
				"	\"url\": \""+url+"\",\n"+
				"}\n"+
				"]";
	}
	
	/**
	* Remove all white spaces and non-visible characters
	* @param str
	* @return
	*/
	public static String removeWhiteSpace(String string) {
		return string.replaceAll("\\s+","");
	}
	
	/**
	* Returns the test Sparql update.
	* 
	* @return UpdateRequest
	* @throws ParseException
	*/
	public static String getUpdateRequest() throws ParseException {
	
		//DELETE {?s ?p ?o} 
		//INSERT {?s ?p \"TEST\" } 
		//WHERE {?s ?p ?o.
		//		 FILTER(?s = <http://www.example.com/test/s> && ?p = <http://www.example.com/test/p>)}
		
		WhereBuilder where = new WhereBuilder()
				.addWhere("?s", "?p", "?o")
				.addFilter("?s = <http://www.example.com/test/s> && ?p = <http://www.example.com/test/p>");
		
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
		
		// Add where 
		builder.addInsert("?s", "?p", "TEST")
			.addDelete("?s", "?p", "?o")
			.addWhere(where);
		
		return builder.buildRequest().toString();
	}
}
