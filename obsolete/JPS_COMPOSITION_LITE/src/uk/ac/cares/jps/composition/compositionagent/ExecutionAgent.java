package uk.ac.cares.jps.composition.compositionagent;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cares.jps.composition.utils.Convertor;

public class ExecutionAgent {

	public CompositionResult composition_result;
	public Map<String, String> http_mapping;
	public JSONObject whole_input_bundle;
	public Map<String, Map<String, String>> input_mapping;

	public ArrayList<JSONObject> data_collection;

	public ExecutionAgent(CompositionResult _composition_result) {
		this.composition_result = _composition_result;
		this.http_mapping = new HashMap<String, String>();
		this.input_mapping = this.composition_result.input_mapping;
		this.data_collection = new ArrayList<JSONObject>();

		this.query_grounding_information();
	}

//	{
//	  "source": "input",	
//	  "mapping": {
//	    "http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType": "region",
//		"http://....": "xxx"  
//	  },
//	  "data": {
//	    "region": {
//	      "lowercorner": {
//	        "lowerx": "4.28946699658502",
//	        "lowery": "52.073035793909966"
//	      },
//	      "uppercorner": {
//	        "upperx": "4.293329377566465",
//	        "uppery": "52.0782056207232"
//	      },
//	      "srsname": "EPSG:4326"
//	    }
//	  }
//	}

	public void execute(JSONObject input) throws JSONException, IOException {

		ArrayList<ArrayList<String>> layers = this.composition_result.layers;
		ArrayList<String> initial_layer = layers.get(0);
		this.data_collection.add(input);
		JSONObject input_json = input.getJSONObject("data");

		for (String agent_iri : initial_layer) {
			String http_url = this.http_mapping.get(agent_iri);
			URIBuilder builder = new URIBuilder().setScheme("http").setHost("www.theworldavatar.com").setPort(80)
					.setPath(http_url.replaceAll("http://www.theworldavatar.com", ""))
					.setParameter("query", input_json.toString());
			JSONObject result = executeGet(builder);

			JSONObject new_result_to_append = new JSONObject();
			new_result_to_append.put("source", agent_iri);
			new_result_to_append.put("data", result);
			this.data_collection.add(new_result_to_append);
		}

		for (int i = 1; i < layers.size(); i++) {
			for (String agent_iri : layers.get(i)) {
				String http_url = this.http_mapping.get(agent_iri);
				Map<String, String> inputs_of_target_agent = this.input_mapping.get(agent_iri);

				JSONObject input_JSON_for_target_agent = new JSONObject(); // This object stores all the input data for
																			// invoking this agent
				for (Map.Entry<String, String> entry : inputs_of_target_agent.entrySet()) {
					String _type = entry.getKey();
					String _key = entry.getValue();

// ================= iterate through data collected from upstream sources including inputs ==========================
					Map<String, String> output_mapping_from_upstream = new HashMap<String, String>();
					for (JSONObject _data : this.data_collection) {
						// if the data collected is from input ...

						String source = _data.getString("source");
						if (source.equalsIgnoreCase("input")) {
							output_mapping_from_upstream = Convertor
									.deserializejsonobject(_data.getJSONObject("mapping"));

						} else {
							output_mapping_from_upstream = this.composition_result.output_mapping.get(source);
						}
						if (output_mapping_from_upstream.containsKey(_type)) {
							// The target agent needs this piece of data...
							String _key_in_upstream = output_mapping_from_upstream.get(_type);
							JSONObject _data_bundle_from_upstream = _data.getJSONObject("data");
							Object _data_part = _data_bundle_from_upstream.get(_key_in_upstream);
							input_JSON_for_target_agent.put(_key_in_upstream, _data_part);
						}
					}
				}

				System.out.println("for agent : " + agent_iri);
				System.out.println("http to be: " + http_url);
				System.out.println("data is   : " + input_JSON_for_target_agent.toString(4));
				URIBuilder builder = new URIBuilder().setScheme("http").setHost("www.theworldavatar.com").setPort(80)
						.setPath(http_url.replaceAll("http://www.theworldavatar.com", ""))
						.setParameter("query", input_json.toString());
				JSONObject result = executeGet(builder);
				System.out.println("it yields : " + result.toString(4));
				System.out.println("--------------------------------------------------------------------------");

			}
		}

	}

	public static JSONObject executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			String result = EntityUtils.toString(httpResponse.getEntity());
//			System.out.println("============ result =============");
//			System.out.println(result);
//			System.out.println("=================================");

			return new JSONObject(result);

		} catch (Exception e) {
		}
		return null;
	}

	public void query_grounding_information() {
		String query = "PREFIX msm:<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#> \r\n"
				+ "PREFIX ontoagent: <http://www.theworldavatar.com/ontology/OntoAgent.owl#>\r\n"
				+ "SELECT  ?agent ?url\r\n" + "WHERE \r\n" + "{" + " ?agent msm:hasOperation ?operation .\r\n"
				+ "  ?operation msm:hasHttpUrl ?url .\r\n}";

		// Make SPARQL query to retrieve grounding information for agent invocation
		QueryExecution qe_up = QueryExecutionFactory
				.sparqlService("http://www.theworldavatar.com/damecoolquestion/agents/query", query);

		ResultSet invocation_info = qe_up.execSelect();
		// ResultSetFormatter.out(invocation_info);
		while (invocation_info.hasNext()) {
			QuerySolution result = invocation_info.next();
			String agent_iri = result.get("agent").toString();
			String url = result.get("url").toString();
			this.http_mapping.put(agent_iri, url);
		}
	}
}
