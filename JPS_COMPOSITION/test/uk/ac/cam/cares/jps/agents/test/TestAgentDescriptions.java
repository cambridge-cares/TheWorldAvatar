package uk.ac.cam.cares.jps.agents.test;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;

import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.agents.ontology.ServiceBuilder;
import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.agents.ontology.ServiceWriter;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestAgentDescriptions extends TestCase {

	private static final String JPS = "http://www.theworldavatar.com/JPS";
	private static final String JPS_BASE = "http://www.theworldavatar.com/JPS_BASE";
	private static final String JPS_CO2EMISSIONS = "http://www.theworldavatar.com/JPS_CO2EMISSIONS";
	private static final String JPS_COMPOSITION = "http://www.theworldavatar.com/JPS_COMPOSITION";
	private static final String JPS_MEN = "http://www.theworldavatar.com/JPS_MEN";
	private static final String JPS_POWSYS = "http://www.theworldavatar.com/JPS_POWSYS";
	private static final String JPS_SCENARIO = "http://www.theworldavatar.com/JPS_SCENARIO";
	private static final String JPS_DES = "http://www.theworldavatar.com/JPS_DES";
	private static final String JPS_ESS = "http://www.theworldavatar.com/JPS_ESS";
	
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
	
	public void testCreateDescription() throws URISyntaxException, FileNotFoundException {
		
		Service service = createDescrForAgentRegionToCity();
		backAndforthAndWrite(service, "_RegionToCity");
		service = createDescrForAgentGetPlantsInRegion();
		backAndforthAndWrite(service, "_GetPlantsInRegion");
		service = createDescrForAgentWeather();
		backAndforthAndWrite(service, "_OpenWeatherMap");
		service = createDescrForAgentPowerPlant();
		backAndforthAndWrite(service, "_PowerPlant");
		service = createDescrForAgentShip();
		backAndforthAndWrite(service, "_Ship");
		service = createDescrForAgentBuildingQuery();
		backAndforthAndWrite(service, "_BuildingQuery");
		service = createDescrForAgentADMS();
		backAndforthAndWrite(service, "_ADMS");
		service = createDescrForComposedAgentADMS();
		backAndforthAndWrite(service, "_ComposedADMS");
		service = createDescrForFactorModel();
		backAndforthAndWrite(service, "_FactorModel");
		service = createDescrForSurrogateModel();
		backAndforthAndWrite(service, "_SurrogateModel");
		service = createDescrForAgentNuclearPP();
		backAndforthAndWrite(service, "_NuclearAgent_startsimulation");
		service=createDescrForAgentMEN();
		backAndforthAndWrite(service, "_MEN");
		service=createDescrForAgentSRMEmissions();
		backAndforthAndWrite(service, "_SRM");
		service=createDescrForAgentCarbonTax();
		backAndforthAndWrite(service, "_CarbonTax");
		service=createDescrForAgentDistributionEnergySystem();
		backAndforthAndWrite(service, "_DES");
		service=createDescrForAgentWeatherRetriever();
		backAndforthAndWrite(service, "_SingporeWeatherDataRetriever");
		
	}
	
	public static void backAndforthAndWrite(Service service, String name) throws URISyntaxException, FileNotFoundException {
		
		new ServiceWriter().writeAsOwlFile(service, name, "C://JPS_DATA/workingdir/JPS_COMPOSITION/testagents");
		
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
	
	private Service createDescrForAgentPowerPlant() {
		return new ServiceBuilder().operation(null, JPS + "/PowerPlant")
			.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
			.input("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism", "reactionmechanism")
			.output("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct", "waste")
			.build();
	}
	
	private Service createDescrForAgentEN() {
		return new ServiceBuilder()
				.operation(null, JPS_POWSYS + "/ENAgent/startsimulationPF")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
				.operation(null, JPS_POWSYS + "/ENAgent/startsimulationOPF")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
			
			.build();
	}
	
	private Service createDescrForAgentShip() {
		return new ServiceBuilder().operation(null, JPS + "/ShipAgent")
			.input("http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#Ship", "ship")
			.input("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism", "reactionmechanism")
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
			.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct", "waste")
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
	
	private Service createDescrForFactorModel() {
		return new ServiceBuilder().operation(null, JPS_CO2EMISSIONS + "/FactorModel")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.output("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission", "hasEmission").down()
				.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue", "hasValue").down()
					.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue", "numericalValue").up()
				.up()
			.build();
	}
	
	private Service createDescrForSurrogateModel() {
		return new ServiceBuilder().operation(null, JPS_CO2EMISSIONS + "/SurrogateModel")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.output("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission", "hasEmission").down()
				.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue", "hasValue").down()
					.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue", "numericalValue").up()
				.up()
			.build();
	}
	
	private Service createDescrForAgentEmissionTest() { //????
		return new ServiceBuilder()
			.operation(null, JPS_BASE + "/EmissionTestAgent/getemission")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.output("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission", "hasEmission").down()
				.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue", "hasValue").down()
					.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue", "numericalValue").up()
				.up()
			.operation(null, JPS_BASE + "/EmissionTestAgent/setemission")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.input("http://www.w3.org/2001/XMLSchema#double", "emission")
			.operation(null, JPS_BASE + "/EmissionTestAgent/add")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.input("http://www.w3.org/2001/XMLSchema#double", "increment")
			.operation(null, JPS_BASE + "/EmissionTestAgent/multiply")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.input("http://www.w3.org/2001/XMLSchema#double", "factor")
			.operation(null, JPS_BASE + "/EmissionTestAgent/change")
			.input("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl", "plant")
			.input("http://www.w3.org/2001/XMLSchema#string", "formula")		
			.build();
	}
	
	private Service createDescrForAgentNuclearPP() {
		return new ServiceBuilder()
			.operation(null, JPS_POWSYS + "/NuclearAgent/startsimulation")
			.input("http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#Landlot", "landlot")
			.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
			.input("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator", true, "substitutionalgenerators", true)
			.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant",true,"plants",true)
//			.operation(null, JPS_POWSYS + "/NuclearAgent/processresult")
//			.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
//			.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant",true,"plants",true)
			.build();
	}
	
	private Service createDescrForRetrofit() {
		return new ServiceBuilder()
			.operation(null, JPS_POWSYS + "/retrofit")
			.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
			.input("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant", "plants")
			.input("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator", true, "substitutionalgenerators", true)
			.output("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
//			.operation(null, JPS_POWSYS + "/NuclearAgent/processresult")
//			.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
//			.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant",true,"plants",true)
			.build();
	}

	private Service createDescrForAgentScenario() { 
		return new ServiceBuilder()
			.operation(null, JPS_SCENARIO + "/mock")
				.input("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service", "scenarioagent")
				//.input("http://www.w3.org/2001/XMLSchema#string", "scenarioname")
			.operation(null, JPS_SCENARIO + "/call")
				.input("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl", "scenarioagentoperation")
				.input("http://www.w3.org/2001/XMLSchema#string", "usecaseurl")
			.operation(null, JPS_SCENARIO + "/read")
				.input("http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource", "scenarioresource")
				//.input("http://www.w3.org/2001/XMLSchema#string", "scenarioname")
				.output("http://www.w3.org/2001/XMLSchema#string", "hasOutput")
			.operation(null, JPS_SCENARIO + "/query")
				.input("http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource", "scenarioresource")
				.input("http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#SparqlQuery", "sparqlquery")
				//.input("http://www.w3.org/2001/XMLSchema#string", "scenarioname")
				.output("http://www.w3.org/2001/XMLSchema#string", "hasOutput")
			.operation(null, JPS_SCENARIO + "/delete")
				//.input("http://www.w3.org/2001/XMLSchema#string", "scenarioname")
				.input("http://www.theworldavatar.com/ontology/ontoagent/OntoAgent.owl#Resource", "scenarioresource")
//			.operation(null, JPS_SCENARIO + "/option")
//				.input("http://www.w3.org/2001/XMLSchema#boolean", JPSConstants.SCENARIO_OPTION_COPY_ON_READ)
			.build();
	}
	
	private Service createDescrForComposedAgentADMS() {
		return new ServiceBuilder()
				.composed()
				.operation(null, null)
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.input("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism", "reactionmechanism")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
	}
	
	private Service createDescrForAgentADMSWithScenarioTest() {//????
		return new ServiceBuilder()
				.operation(null, JPS + "/ADMSCoordinationAgentWithScenario")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.input("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism", "reactionmechanism")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
	}
	
	private Service createDescrForAgentMEN() {
		return new ServiceBuilder()
				.operation(null, JPS_MEN + "/MENAgent")
				.input("http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#Eco-industrialPark", "ecoindustrialpark")
				.input("http://www.theworldavatar.com/ontology/Market.owl#Price", "carbontax")
				.input("http://www.theworldavatar.com/ontology/Market.owl#InterestFactor", "interestfactor")
				.input("http://www.theworldavatar.com/ontology/Market.owl#CostFactor", "annualcostfactor")
				.output("http://www.theworldavatar.com/ontology/Market.owl#Cost", "totalcost")
				.build();
	}
	
	private Service createDescrForAgentSRMEmissions() {
		return new ServiceBuilder()
				.operation(null, JPS + "/SRMAgent")
				.input("http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ReactionMechanism", "reactionmechanism")
				.input("http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#CompressionIgnitionEngine", "engine")
				.output("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct", "waste")
				.build();
	}
		
	private Service createDescrForAgentCarbonTaxCoord() {
		return new ServiceBuilder()
			.operation(null, JPS_POWSYS + "/startsimulation")
			.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
			.input("http://www.theworldavatar.com/ontology/Market.owl#Price", "carbontax")
			.input("http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#Landlot", "landlot")
			.build();
	}
	
	private Service createDescrForAgentCarbonTax() {
		return new ServiceBuilder()
				.operation(null, JPS_POWSYS + "/optimizeforcarbontax")
				.input("http://www.theworldavatar.com/ontology/Market.owl#Price", "carbontax")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
				.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator",true, "substitutionalgenerators",true)
				.build();
	}
	
	private Service createDescrForAgentDistributionEnergySystem() {
		return new ServiceBuilder()
				.operation(null, JPS_DES + "/DESAgent")
				.input("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#District", "district")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
				.output("https://www.w3.org/ns/csvw#Table", "consumptiongrid")
				.build();
	}
	
	private Service createDescrForAgentWeatherRetriever() {
		return new ServiceBuilder()
				.operation(null, JPS_DES + "/GetIrradiationandWeatherData")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#T-Sensor", "tempsensor")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#Q-Sensor", "irradiationsensor")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#F-Sensor", "speedsensor")
				.build();
	}
		private Service createDescrForAgentEnergyStorageSystem() {//the coordination
		return new ServiceBuilder()
				.operation(null, JPS_ESS + "/ESSAgent")
				.input("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator", "RenewableEnergyGenerator")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "electricalnetwork")
				.input("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#CompositeSystem", "BatteryCatalog")
				.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Battery",true, "batterylist",false)
				.output("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#EnergyStorageSystem",true, "batterylist",false)
				.build();
	}
	
	public void testDescription() throws URISyntaxException, FileNotFoundException {
		
		Service service = createDescrForAgentCarbonTaxCoord();
		
		String json = new Gson().toJson(service);
		System.out.println(json);
		
		backAndforthAndWrite(service, "_CarbonTaxCoord");
	}
}
