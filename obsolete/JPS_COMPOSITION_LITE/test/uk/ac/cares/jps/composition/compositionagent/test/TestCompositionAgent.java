package uk.ac.cares.jps.composition.compositionagent.test;

import static org.junit.Assert.assertEquals;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;

import uk.ac.cares.jps.composition.compositionagent.CompositionAgent;

public class TestCompositionAgent {

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void test() throws JSONException, JsonProcessingException {

		JSONArray inputs = new JSONArray();
		inputs.put("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType");
		JSONArray outputs = new JSONArray();
		outputs.put("https://www.w3.org/ns/csvw#Table");
		outputs.put("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType");

		CompositionAgent composition_agent = new CompositionAgent();
		JSONObject result = composition_agent.compose(inputs, outputs);
		// Test case 001, the initial test case, region --> [building,table]
		// assertEquals(result.toString(),			"{\"input_mapping\":{\"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\",\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\":\"weatherstate\",\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\":\"waste\",\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\":\"plant\",\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\":\"region\"},\"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\":\"plant\"},\"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl\":\"plant\"},\"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\":\"region\"},\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\"},\"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\":\"region\"},\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\"},\"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\":\"plant\"},\"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\"},\"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\",\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\":\"region\"},\"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\":\"plant\",\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\":\"region\"}},\"output_mapping\":{\"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\":{\"https://www.w3.org/ns/csvw#Table\":\"dispersiongrid\"},\"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\":\"waste\"},\"http://www.theworldavatar.com/kb/agents/Service__FactorModel.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission\":\"hasEmission\"},\"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant\":\"plant\"},\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\":{\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\":\"weatherstate\"},\"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\":{\"http://dbpedia.org/ontology/city\":\"city\"},\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\":{\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\":\"weatherstate\"},\"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct\":\"waste\"},\"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\":{\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\":\"weatherstate\"},\"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\":{\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\":\"buildings\"},\"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\":{\"https://www.w3.org/ns/csvw#Table\":\"dispersiongrid\",\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\":\"buildings\"}},\"layers\":[[\"http://www.theworldavatar.com/kb/agents/Service__GetPlantsInRegion.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__RegionToCity.owl#Service\"],[\"http://www.theworldavatar.com/kb/agents/Service__SRMEmissions.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__AccuWeather.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__PowerPlant.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__YahooWeather.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__BuildingQuery.owl#Service\",\"http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service\"],[\"http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service\"]]}");
	}
}
