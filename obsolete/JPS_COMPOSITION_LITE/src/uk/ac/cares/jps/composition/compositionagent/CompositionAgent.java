package uk.ac.cares.jps.composition.compositionagent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.core.JsonProcessingException;

import uk.ac.cares.jps.composition.utils.Convertor;

public class CompositionAgent {

	
	
	// 
	public JSONObject compose(JSONArray inputs, JSONArray outputs) throws JSONException, JsonProcessingException {

		
		
		
		ArrayList<ArrayList<String>> layers = new ArrayList<ArrayList<String>>();

		Map<String, Map<String, String>> agents_and_inputs_mapping = query_sparql_endpoint_for_inputs();
		
		// 
		
		
		
		
		
		System.out.println("------------------------agents_and_inputs_mapping ---------------------------");
		System.out.println(agents_and_inputs_mapping);
		System.out.println("-----------------------------------------------------------------------------");

		Map<String, Map<String, String>> agents_and_outputs_mapping = query_sparql_endpoint_for_outputs();

		Set<String> all_collected_inputs = new HashSet<String>();
		Set<String> all_agents_added = new HashSet<String>();

		ArrayList<String> inputs_list = Convertor.convert_JSONArray_to_ArrayList(inputs);
		ArrayList<String> outputs_list = Convertor.convert_JSONArray_to_ArrayList(outputs);

		all_collected_inputs.addAll(new HashSet<String>(inputs_list));

		System.out.println("---------------------------- I/O requirements received -------------------------------");
		System.out.println("inputs  received : " + inputs_list);
		System.out.println("outputs received : " + outputs_list);
		System.out.println("--------------------------------------------------------------------------------------");

		int counter = 0;
		boolean all_outputs_met = false;
		while (counter < 10 && !all_outputs_met) {
			ArrayList<String> this_layer = find_eligible_agents(all_collected_inputs, agents_and_inputs_mapping,
					all_agents_added);
			System.out.println("--------------------- layer " + counter + " contains --------------------------");
			System.out.println(this_layer);
			System.out.println("-------------------------------------------------------------------------------");
			layers.add(this_layer);
			Set<String> new_inputs = get_new_inputs(this_layer, agents_and_outputs_mapping);
			all_outputs_met = all_collected_inputs.containsAll(outputs_list);
			all_collected_inputs.addAll(new_inputs);
			counter++;
		}

		CompositionResult composition_result = new CompositionResult(agents_and_inputs_mapping,
				agents_and_outputs_mapping, layers);

		JSONObject composition_result_in_JSON = Convertor.serialize_composition_result(composition_result);
		
		composition_result_in_JSON.put("initialInputs", inputs_list);
		
		System.out.println("------------------------- composition result ---------------------------");
		System.out.println(composition_result_in_JSON.toString(4));
		System.out.println("------------------------------------------------------------------------");
		return composition_result_in_JSON;

	}

	public static Map<String, Map<String, String>> query_sparql_endpoint_for_outputs() {
		Map<String, Map<String, String>> agents_and_outputs_mapping = new HashMap<String, Map<String, String>>();
		String agent_query_string_for_outputs = ""
				+ "			PREFIX msm:<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#> \r\n"
				+ "			PREFIX ontoagent:<http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#> \r\n"
				+ "			SELECT DISTINCT ?agent ?outputType ?outputKey\r\n" + "				WHERE \r\n"
				+ "					  {     \r\n" + "			  			?agent msm:hasOperation ?operation .\r\n"
				+ "					    ?operation msm:hasOutput ?messageCotentsForOutput .\r\n"
				+ "						?messageCotentsForOutput msm:hasMandatoryPart ?mandatoryPartOutput .\r\n"
				+ "						?mandatoryPartOutput msm:hasType ?outputType .\r\n"
				+ "						?mandatoryPartOutput msm:hasName ?outputKey .\r\n" + "			   		  }";

		QueryExecution qe_for_output = QueryExecutionFactory.sparqlService(
				"http://www.theworldavatar.com/damecoolquestion/agents/query", agent_query_string_for_outputs);
		ResultSet results_of_outputs = qe_for_output.execSelect();
//
//		System.out.println("------------------------- output query ----------------------------");
//		System.out.println(agent_query_string_for_outputs);
//		System.out.println("-------------------------------------------------------------------");

		// ResultSetFormatter.out(results_of_outputs);

		while (results_of_outputs.hasNext()) {
			QuerySolution result = results_of_outputs.next();
			String agent = result.get("agent").toString();
			String outputType = result.get("outputType").toString(); // Add the key and forms an object that contains
																		// both the inputTypes and inputKey ...
			String outputKey = result.get("outputKey").toString();
			Map<String, String> type_key_pair = new HashMap<String, String>();

			if (agents_and_outputs_mapping.containsKey(agent)) {
				agents_and_outputs_mapping.get(agent).put(outputType, outputKey);
			} else {
				Map<String, String> new_pair = new HashMap<String, String>();
				new_pair.put(outputType, outputKey);
				agents_and_outputs_mapping.put(agent, new_pair);
			}
		}
		return agents_and_outputs_mapping;

	}

	public static Map<String, Map<String, String>> query_sparql_endpoint_for_inputs() {

		Map<String, Map<String, String>> agents_and_inputs_mapping = new HashMap<String, Map<String, String>>();

		String agent_query_string_for_inputs = ""
				+ "PREFIX msm:<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#> \r\n"
				+ "PREFIX ontoagent:<http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#> \r\n"
				+ "SELECT DISTINCT ?agent ?inputType ?inputKey \r\n" + "	WHERE \r\n" + "		  {     \r\n"
				+ "  			?agent msm:hasOperation ?operation .\r\n"
				+ "             ?operation msm:hasInput ?messageCotentsForInput .\r\n"
				+ "             ?messageCotentsForInput msm:hasMandatoryPart ?mandatoryPart .\r\n"
				+ "  			?mandatoryPart msm:hasType ?inputType .\r\n"
				+ "  			?mandatoryPart msm:hasName ?inputKey .\r\n" + "}";

		System.out.println("------------------ Agent I/O Query -----------------");
		System.out.println(agent_query_string_for_inputs);
		System.out.println("----------------------------------------------------");

		// The SPARQL query to retrieve the input types of agents
		QueryExecution qe = QueryExecutionFactory.sparqlService(
				"http://www.theworldavatar.com/damecoolquestion/agents/query", agent_query_string_for_inputs);
		ResultSet results = qe.execSelect();
		// ResultSetFormatter.out(results);

		// Fire the SPARQL query
		System.out.println("============================ query from querying inputs ========================");
		while (results.hasNext()) {
			QuerySolution result = results.next();
			String agent = result.get("agent").toString();
			String inputType = result.get("inputType").toString();
			String inputKey = result.get("inputKey").toString();

			System.out.println(agent + "\t\t\t\t\t  |  " + inputType);

			if (agents_and_inputs_mapping.containsKey(agent)) {
				agents_and_inputs_mapping.get(agent).put(inputType, inputKey);
			} else {
				Map<String, String> new_pair = new HashMap<String, String>();
				new_pair.put(inputType, inputKey);
				agents_and_inputs_mapping.put(agent, new_pair);
			}
		}
		System.out.println("===============================================================================");

		return agents_and_inputs_mapping;
	}

	public static ArrayList<String> find_eligible_agents(Set<String> inputs,
			Map<String, Map<String, String>> agents_and_inputs_mapping, Set<String> all_agents_added) {
		ArrayList<String> agent_iris = new ArrayList<String>();

		// Query the SPARQL Endpoint and generate a mapping between agents and their
		// input types

		for (Map.Entry<String, Map<String, String>> entry : agents_and_inputs_mapping.entrySet()) {

			ArrayList<String> temp_input_list = new ArrayList<String>();
			// entry.getValue is an ArrayList<Map<String,String>>

			for (Map.Entry<String, String> this_entry : entry.getValue().entrySet()) {
				String _type = this_entry.getKey();
				temp_input_list.add(_type);
			}

			if (inputs.containsAll(temp_input_list)) {
				// if the agent's inputs is a subset of the inputs required, this agent is
				// considered eligible
				if (!all_agents_added.contains(entry.getKey())) {
					agent_iris.add(entry.getKey());
					all_agents_added.add(entry.getKey());
				}
			}
		}
		return agent_iris;
	}

	public static Set<String> get_new_inputs(ArrayList<String> agent_iris,
			Map<String, Map<String, String>> agents_and_outputs_mapping) {
		ArrayList<String> new_inputs = new ArrayList<String>();
		for (String agent_iri : agent_iris) {
			Map<String, String> outputs = agents_and_outputs_mapping.get(agent_iri);
			for (Map.Entry<String, String> entry : outputs.entrySet()) {
				String _type = entry.getKey();
				new_inputs.add(_type);
			}
		}

		return new HashSet<String>(new_inputs);
	}

}
