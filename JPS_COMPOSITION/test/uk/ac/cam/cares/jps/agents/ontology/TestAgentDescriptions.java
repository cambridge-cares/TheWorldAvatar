package uk.ac.cam.cares.jps.agents.ontology;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestAgentDescriptions extends TestCase {

	private static final String JPS = "http://www.theworldavatar.com/JPS";
	private static final String JPS_COMPOSITION = "http://www.theworldavatar.com/JPS_COMPOSITION";
	private static final String WEATHER = "https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl";
	
	private ServiceBuilder addInputRegion(ServiceBuilder builder) {
		return builder.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region").down()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lowerCornerPoint", "lowercornerpoint").down()
					.input("https://www.w3.org/2001/XMLSchema#double", "lowerx")
					.input("https://www.w3.org/2001/XMLSchema#double", "lowery").up()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#upperCornerPoint", "uppercornerpoint").down()
					.input("https://www.w3.org/2001/XMLSchema#double", "upperx")
					.input("https://www.w3.org/2001/XMLSchema#double", "uppery").up()
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#srsname", "srsname").up();
	}
	
	public void testcreateDescription() throws URISyntaxException, FileNotFoundException {
		
//		Service service = createDescrForAgentRegionToCity();
//		backAndforthAndWrite(service, "RegionToCity");
//		service = createDescrForAgentGetPlantsInRegion();
//		backAndforthAndWrite(service, "GetPlantsInRegion");
//		service = createDescrForAgentWeather();
//		backAndforthAndWrite(service, "OpenWeatherMap");
//		service = createDescrForAgentGetWindDirection();
//		backAndforthAndWrite(service, "GetWindDirection");

		Service service = createDescrForADMSAgent();
		backAndforthAndWrite(service, "ADMSAgent");
	}
	
	private void backAndforthAndWrite(Service service, String name) throws URISyntaxException, FileNotFoundException {
		
		new ServiceWriter().writeAsOwlFile(service, name, "C:\\Users\\nasac\\Documents\\TMP\\newAgentsMSM");
		
		
		
		service.setUri(null);
		String owlService = new ServiceWriter().generateSerializedModel(service, name);
		
		System.out.println("=======================================");
		System.out.println(owlService);
		System.out.println("=======================================");
		
		new ServiceReader().parse(owlService, null).get(0);	
	}
	
	private Service createDescrForAgentRegionToCity() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/RegionToCity"))
			.output("http://dbpedia.org/ontology/city", "city")
			.build();
	}
	
	private Service createDescrForAgentGetPlantsInRegion() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/GetPlantsInRegion"))
			.output("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant", true, "plant", true)
			.build();
	}
	
	private Service createDescrForAgentWeather() {
		ServiceBuilder builder = new ServiceBuilder().operation(null, JPS_COMPOSITION + "/CityToWeather")
			.input("http://dbpedia.org/ontology/city", "city")
			.output(WEATHER + "#WeatherState", "weatherstate").down()
				.output(WEATHER + "#hasHumidity", "humidity").down()
					.output(WEATHER + "#hasValue", "value").up()
				.output(WEATHER + "#hasExteriorTemperature", "exteriortemperature").down()
					.output(WEATHER + "#hasValue", "value").up()
				.output(WEATHER + "#hasWind", "haswind").down()
					.output(WEATHER + "#hasSpeed", "speed")
					.output(WEATHER + "#hasDirection", "direction").up()
				.output(WEATHER + "#hasWeatherCondition", "weathercondition") // not required for ADMS
				.output(WEATHER + "#hasCloudCover", "cloudcover").down()
					.output(WEATHER + "#hasCloudCoverValue", "cloudcovervalue").up()
				.output(WEATHER + "#hasPrecipitation", "precipation").down()
					.output(WEATHER + "#hasIntensity", "intensity").up()
				.up();
		
		return builder.build();
	}
	
	private Service createDescrForADMSAgent() {

		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/ADMSAgent"))
				.input("http://dbpedia.org/ontology/city", "city")
				.input("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant",true, "plant",true)
				.input(WEATHER + "#WeatherState", "weatherstate").down()
				.input(WEATHER + "#hasHumidity", "hashumidity").down()
					.input(WEATHER + "#hasValue", "hasvalue").up()
				.input(WEATHER + "#hasExteriorTemperature", "hasexteriortemperature").down()
					.input(WEATHER + "#hasValue", "hasvalue").up()
				.input(WEATHER + "#hasWind", "haswind").down()
					.input(WEATHER + "#hasSpeed", "hasspeed")
					.input(WEATHER + "#hasDirection", "hasdirection").up()
				.input(WEATHER + "#hasWeatherCondition", "hasweathercondition") // not required for ADMS
				.input(WEATHER + "#hasCloudCover", "hascloudcover").down()
					.input(WEATHER + "#hasCloudCoverValue", "hascloudcovervalue").up()
				.input(WEATHER + "#hasPrecipitation", "hasprecipation").down()
					.input(WEATHER + "#hasIntensity", "hasintensity").up()
				.output("http://test.com/ontology/ADMSSimulation", "admssimulation")
				.build();
		
	}
	 
	
	
	
	private Service createDescrForAgentGetWindDirection() {
		// This virtual agent takes weather data and separate the wind direction .
		
		ServiceBuilder builder = new ServiceBuilder().operation(null, JPS_COMPOSITION + "/WeatherToWindDirection")
				.input(WEATHER + "WeatherState", "currentweatherstate").down()
				.input(WEATHER + "#hasHumidity", "humidity").down()
					.input(WEATHER + "#hasValue", "value").up()
				.input(WEATHER + "#hasExteriorTemperature", "hastemperature").down()
					.input(WEATHER + "#hasValue", "value").up()
				.input(WEATHER + "#hasWind", "haswind").down()
					.input(WEATHER + "#hasSpeed", "speed")
					.input(WEATHER + "#hasDirection", "direction").up()
				.input(WEATHER + "#hasWeatherCondition", "heathercondition") // not required for ADMS
				.input(WEATHER + "#hasCloudCover", "cloudcover").down()
					.input(WEATHER + "#hasCloudCoverValue", "cloudcovervalue").up()
				.input(WEATHER + "#hasPrecipitation", "precipation").down()
					.input(WEATHER + "#hasIntensity", "intensity").up()
				.up()
				.output(WEATHER + "#hasDirection","winddirection");
		
		return builder.build();
	}
	
 	public void testWeatherDescription() throws URISyntaxException, FileNotFoundException {
		
		Service service1 = createDescrForAgentWeather();
		Service service2 = createDescrForAgentGetWindDirection();
		String json1 = new Gson().toJson(service1);
		String json2 = new Gson().toJson(service2);
		System.out.println("--------------------------------------------");
		System.out.println(json2);
		
		
		// backAndforthAndWrite(service, "OpenWeatherMap");
	}
}
