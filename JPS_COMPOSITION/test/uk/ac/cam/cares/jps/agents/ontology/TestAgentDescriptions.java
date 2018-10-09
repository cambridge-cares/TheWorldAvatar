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
		
		Service service = createDescrForAgentRegionToCity();
		backAndforthAndWrite(service, "_RegionToCity");
		service = createDescrForAgentGetPlantsInRegion();
		backAndforthAndWrite(service, "_GetPlantsInRegion");
		service = createDescrForAgentWeather();
		backAndforthAndWrite(service, "_OpenWeatherMap");
		service = createDescrForAgentSRMEmissions();
		backAndforthAndWrite(service, "_SRMEmissions");
		service = createDescrForAgentBuildingQuery();
		backAndforthAndWrite(service, "_BuildingQuery");
		service = createDescrForAgentADMS();
		backAndforthAndWrite(service, "_ADMS");
	}
	
	private void backAndforthAndWrite(Service service, String name) throws URISyntaxException, FileNotFoundException {
		
		new ServiceWriter().writeAsOwlFile(service, name, "C:\\Users\\nasac\\Documents\\TMP\\newAgentsMSM");
		//new ServiceWriter().writeAsOwlFile(service, name, "C:\\Users\\Andreas\\TMP\\newAgentsMSM");
		
		
		service.setUri(null);
		String owlService = new ServiceWriter().generateSerializedModel(service, name);
		
		System.out.println();
		System.out.println(owlService);
		System.out.println();
		
		new ServiceReader().parse(owlService, null).get(0);	
	}
	
	private Service createDescrForAgentRegionToCity() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/RegionToCity"))
			.output("http://dbpedia.org/ontology/city", "city")
			.build();
	}
	
	private Service createDescrForAgentGetPlantsInRegion() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/GetPlantsInRegion"))
			.output("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", true, "plant", true)
			.build();
	}
	
	private Service createDescrForAgentWeather() {
		ServiceBuilder builder = new ServiceBuilder().operation(null, JPS_COMPOSITION + "/CityToWeather")
			.input("http://dbpedia.org/ontology/city", "city")
			.output(WEATHER + "#WeatherState", "weatherstate").down()
				.output(WEATHER + "#hasHumidity", "hashumidity").down()
					.output(WEATHER + "#hasValue", "hasvalue").up()
				.output(WEATHER + "#hasExteriorTemperature", "hasexteriortemperature").down()
					.output(WEATHER + "#hasValue", "hasvalue").up()
				.output(WEATHER + "#hasWind", "haswind").down()
					.output(WEATHER + "#hasSpeed", "hasspeed")
					.output(WEATHER + "#hasDirection", "hasdirection").up()
				.output(WEATHER + "#hasWeatherCondition", "hasweathercondition") // not required for ADMS
				.output(WEATHER + "#hasCloudCover", "hascloudcover").down()
					.output(WEATHER + "#hasCloudCoverValue", "hascloudcovervalue").up()
				.output(WEATHER + "#hasPrecipitation", "hasprecipation").down()
					.output(WEATHER + "#hasIntensity", "hasintensity").up()
				.up();
		
		return builder.build();
	}
	
	private Service createDescrForAgentSRMEmissions() {
		return new ServiceBuilder().operation(null, JPS + "/calculateEmissionStream")
			.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
			.output("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct", "waste")
			.build();
	}
	
	private Service createDescrForAgentBuildingQuery() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/GetBuildingListFromRegion"))
			.input("http://dbpedia.org/ontology/city", "city")
			.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
			.build();
	}
	
	private Service createDescrForAgentADMS() {
		return addInputRegion(new ServiceBuilder().operation(null, JPS + "/ADMSAgent"))
			.input("http://dbpedia.org/ontology/city", "city")
			.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
			//.input("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct", "waste")
			.input(WEATHER + "#WeatherState", "weatherstate").down()
			.input(WEATHER + "#hasHumidity", "humidity").down()
				.input(WEATHER + "#hasValue", "value").up()
			.input(WEATHER + "#hasExteriorTemperature", "exteriortemperature").down()
				.input(WEATHER + "#hasValue", "value").up()
			.input(WEATHER + "#hasWind", "haswind").down()
				.input(WEATHER + "#hasSpeed", "speed")
				.input(WEATHER + "#hasDirection", "direction").up()
			.input(WEATHER + "#hasWeatherCondition", "weathercondition") // not required for ADMS
			.input(WEATHER + "#hasCloudCover", "cloudcover").down()
				.input(WEATHER + "#hasCloudCoverValue", "cloudcovervalue").up()
			.input(WEATHER + "#hasPrecipitation", "precipation").down()
				.input(WEATHER + "#hasIntensity", "intensity").up()	
			.up()
			.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
			.build();
	}
	
	public void testDescription() throws URISyntaxException, FileNotFoundException {
		
		Service service = createDescrForAgentADMS(); 
		
		String json = new Gson().toJson(service);
		System.out.println(json);
	}
}
