package uk.ac.cares.jps.composition.compositionagent.test;

import java.io.IOException;
import java.util.ArrayList;

import org.json.JSONObject;
import org.junit.After;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cares.jps.composition.compositionagent.CompositionResult;
import uk.ac.cares.jps.composition.compositionagent.ExecutionAgent;
import uk.ac.cares.jps.composition.utils.Convertor;

public class TestExecutionAgent {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws JsonParseException, JsonMappingException, IOException {

		// The execution agent receives two set of inputs:
		// 1. serialized composition result
		// 2. inputs in such a form:

//		{
//			  "source": "input",	
//			  "mapping": {
//			    "http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType": "region"
//			  },
//			  "data": {
//			    "region": {
//			      "lowercorner": {
//			        "lowerx": "476951.216",
//			        "lowery": "6813210"
//			      },
//			      "uppercorner": {
//			        "upperx": "476961.216",
//			        "uppery": "6813220"
//			      },
//			      "srsname": "EPSG:3857"
//			    }
//			  }
//			}

		String initial_input = "		{\r\n" + "			  \"source\": \"input\",	\r\n"
				+ "			  \"mapping\": {\r\n"
				+ "			    \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"\r\n"
				+ "			  },\r\n" + "			  \"data\": {\r\n" + "			    \"region\": {\r\n"
				+ "			      \"lowercorner\": {\r\n" + "			        \"lowerx\": \"476951.216\",\r\n"
				+ "			        \"lowery\": \"6813210\"\r\n" + "			      },\r\n"
				+ "			      \"uppercorner\": {\r\n" + "			        \"upperx\": \"476961.216\",\r\n"
				+ "			        \"uppery\": \"6813220\"\r\n" + "			      },\r\n"
				+ "			      \"srsname\": \"EPSG:3857\"\r\n" + "			    }\r\n" + "			  }\r\n"
				+ "			}";
		String compositionResultInString = "{\r\n" + "    \"input_mapping\": {\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\": {\r\n"
				+ "            \"http://dbpedia.org/ontology/city\": \"city\",\r\n"
				+ "            \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\": \"weatherstate\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\": \"waste\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\": \"plant\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"\r\n"
				+ "        },\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\": \"plant\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl\": \"plant\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\": {\"http://dbpedia.org/ontology/city\": \"city\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\": {\"http://dbpedia.org/ontology/city\": \"city\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\": \"plant\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\": {\"http://dbpedia.org/ontology/city\": \"city\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\": {\r\n"
				+ "            \"http://dbpedia.org/ontology/city\": \"city\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"\r\n"
				+ "        },\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\": {\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism\": \"reactionmechanism\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\": \"plant\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\": \"region\"\r\n"
				+ "        }\r\n" + "    },\r\n" + "    \"output_mapping\": {\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\": {\"https://www.w3.org/ns/csvw#Table\": \"dispersiongrid\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\": \"waste\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission\": \"hasEmission\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\": \"plant\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\": {\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\": \"weatherstate\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\": {\"http://dbpedia.org/ontology/city\": \"city\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\": {\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\": \"weatherstate\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\": \"waste\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\": {\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\": \"weatherstate\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\": {\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\": \"buildings\"},\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\": {\r\n"
				+ "            \"https://www.w3.org/ns/csvw#Table\": \"dispersiongrid\",\r\n"
				+ "            \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\": \"buildings\"\r\n"
				+ "        }\r\n" + "    },\r\n" + "    \"layers\": [\r\n" + "        [\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\"\r\n"
				+ "        ],\r\n" + "        [\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\"\r\n"
				+ "        ],\r\n"
				+ "        [\"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\"],\r\n"
				+ "        []\r\n" + "    ]\r\n" + "}";
		JSONObject composition_result_in_JSON = new JSONObject(compositionResultInString);
		CompositionResult composition_result = Convertor.deserialize_composition_result(composition_result_in_JSON);
		ExecutionAgent execution_agent = new ExecutionAgent(composition_result);
		execution_agent.execute(new JSONObject(initial_input));

	}

	public static void execute_one_layer(ArrayList<String> layer, JSONObject all_inputs) {
		// What do you need?
		// Every result should contain the following information:
		/*
		 * [ { "mapping": {
		 * "http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType":
		 * "region" } "data": { "region": {xxx}, "weather": {xxx} } } ]
		 */
	}

}
