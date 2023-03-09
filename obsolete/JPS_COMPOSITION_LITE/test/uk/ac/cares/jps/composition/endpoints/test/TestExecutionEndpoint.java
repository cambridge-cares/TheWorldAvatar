package uk.ac.cares.jps.composition.endpoints.test;

import org.apache.http.client.utils.URIBuilder;
import org.junit.After;
import org.junit.Test;

import uk.ac.cares.jps.composition.utils.Request;

public class TestExecutionEndpoint {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {

		String compositionResultInString = "" + "{\r\n" + "    \"input_mapping\": {\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\": [\r\n"
				+ "            {\r\n"
				+ "                \"type\": \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\",\r\n"
				+ "                \"key\": \"weatherstate\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\",\r\n"
				+ "                \"key\": \"waste\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\",\r\n"
				+ "                \"key\": \"plant\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "                \"key\": \"city\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "                \"key\": \"region\"\r\n" + "            }\r\n" + "        ],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\",\r\n"
				+ "            \"key\": \"plant\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl\",\r\n"
				+ "            \"key\": \"plant\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "            \"key\": \"region\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "            \"key\": \"city\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "            \"key\": \"region\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "            \"key\": \"city\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\",\r\n"
				+ "            \"key\": \"plant\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "            \"key\": \"city\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\": [\r\n"
				+ "            {\r\n" + "                \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "                \"key\": \"city\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "                \"key\": \"region\"\r\n" + "            }\r\n" + "        ],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\": [\r\n"
				+ "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\",\r\n"
				+ "                \"key\": \"plant\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\r\n"
				+ "                \"key\": \"region\"\r\n" + "            }\r\n" + "        ]\r\n" + "    },\r\n"
				+ "    \"output_mapping\": {\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\": [{\r\n"
				+ "            \"type\": \"https://www.w3.org/ns/csvw#Table\",\r\n"
				+ "            \"key\": \"dispersiongrid\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\",\r\n"
				+ "            \"key\": \"waste\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission\",\r\n"
				+ "            \"key\": \"hasEmission\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\",\r\n"
				+ "            \"key\": \"plant\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\": [{\r\n"
				+ "            \"type\": \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\",\r\n"
				+ "            \"key\": \"weatherstate\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://dbpedia.org/ontology/city\",\r\n"
				+ "            \"key\": \"city\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\": [{\r\n"
				+ "            \"type\": \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\",\r\n"
				+ "            \"key\": \"weatherstate\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\",\r\n"
				+ "            \"key\": \"waste\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\": [{\r\n"
				+ "            \"type\": \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\",\r\n"
				+ "            \"key\": \"weatherstate\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\": [{\r\n"
				+ "            \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\",\r\n"
				+ "            \"key\": \"buildings\"\r\n" + "        }],\r\n"
				+ "        \"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\": [\r\n"
				+ "            {\r\n"
				+ "                \"type\": \"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\",\r\n"
				+ "                \"key\": \"buildings\"\r\n" + "            },\r\n" + "            {\r\n"
				+ "                \"type\": \"https://www.w3.org/ns/csvw#Table\",\r\n"
				+ "                \"key\": \"dispersiongrid\"\r\n" + "            }\r\n" + "        ]\r\n"
				+ "    },\r\n" + "    \"layers\": [\r\n" + "        [\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\"\r\n"
				+ "        ],\r\n" + "        [\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\",\r\n"
				+ "            \"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\"\r\n"
				+ "        ],\r\n"
				+ "        [\"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\"]\r\n" + "    ]\r\n"
				+ "}";

		String myHost = "localhost";
		int myPort = 8080;
		URIBuilder builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath("/JPS_COMPOSITION_LITE/ExecutionEndpoint").setParameter("query", compositionResultInString);
		String result = Request.executeGet(builder);
		System.out.println("result= "+result);
	}

}
