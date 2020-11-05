package uk.ac.cam.cares.jps.market;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

public class DataDownload {
	private static Logger logger = LoggerFactory.getLogger(DataDownload.class);
	
	public static final String CPO_DOWNLOAD = "caresjpsarbitrage/CPO_download.pyw";
	public static final String CPO_PAGE = "http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075";
	
	public static final String FAME_DOWNLOAD = "caresjpsarbitrage/FAME_download.pyw";
	public static final String FAME_PAGE = "http://www.cmegroup.com/trading/energy/refined-products/fame-0-argus-biodiesel-fob-rdam-red-compliant-swap-futures.html";
	
	public static final String ZCE_DOWNLOAD = "caresjpsarbitrage/ZCE_download.pyw";
	public static final String ZCE_PAGE = "http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm";
	
	public static final String HNG_DOWNLOAD = "caresjpsarbitrage/HNG_download.pyw";
	public static final String HNG_PAGE = "http://www.cmegroup.com/trading/energy/natural-gas/natural-gas.html";
	
	// URIs of ontologies used to define KBs in which market data will be stored
	public static final String ONTO_PATH_ELEC_MARKETS = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";	
	public static final String ONTO_PATH_KB_MARKETS = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";
//	public static final String ONTO_PATH_KB_MARKETS = "http://www.theworldavatar.com/OntoArbitrage/OntoArbitrage_Market_KB.owl";
	
	// URIs of ontologies used to defined KBs in which utilities and exchange rates will be stored
	public static final String ONTO_PATH_ONTOCAPE = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl";
	public static final String ONTO_PATH_KB_UTIL_EXRATES = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15";
//	public static final String ONTO_PATH_KB_UTIL_EXRATES = "http://www.theworldavatar.com/OntoArbitrage/OntoArbitrage_PlantInfo_KB.owl";
	
	public static String downloadMarketData(String script, String source) throws Exception {
		String result = "";
		do {
			String path = AgentLocator.getCurrentJpsAppDirectory(new DataDownload());
			
			result = CommandHelper.executeSingleCommand( path, "python " + path + " " + source);
		} while (result.equals("retry"));
		
		return result;
	}
	
	public static String downloadCPOMarketData() throws Exception {
		return downloadMarketData(CPO_DOWNLOAD, CPO_PAGE);
	}
	
	public static String downloadFAMEMarketData() throws Exception {
		return downloadMarketData(FAME_DOWNLOAD, FAME_PAGE);
	}
	
	public static String downloadZCEMarketData() throws Exception {
		return downloadMarketData(ZCE_DOWNLOAD, ZCE_PAGE);
	}
	
	public static String downloadHNGMarketData() throws Exception {
		return downloadMarketData(HNG_DOWNLOAD, HNG_PAGE);
	}
	
	public static void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);

		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		System.out.println("File saved with " + errors.size() + " errors.");
	}
	

	
	/**
	 * this function calls cmd to execute 2 Python scripts
	 * which download market prices for crude palm oil
	 * (CPO) and biodiesel (FAME), or natural gas at Henry Hub
	 * (HNG) and methanol at Zhengzhou exchange (ZCE) and
	 * stores it JPS knowledge base; prices of CPO are
	 * captured from cmd and returned for testing purposes
	 * 
	 * @return
	 * @throws Exception
	 */
	public static String downloadingAndSavingMarketDataInTheKnowledgeBase(String choicePlant)
			throws Exception {
		
		String[] results = new String[2];
		String[][] addresses = null;
		
		if (choicePlant.equals("Biodiesel")) {
			results[0] = downloadCPOMarketData();
			logger.info(results[0]);
			results[1] = downloadFAMEMarketData();
			logger.info(results[1]);
			
			addresses = new String[][] {
				{ ONTO_PATH_ELEC_MARKETS + "#" + "data",
				  ONTO_PATH_KB_MARKETS + "#" + "CMECrudePalmOil_001" },
				{ ONTO_PATH_ELEC_MARKETS + "#" + "data",
				  ONTO_PATH_KB_MARKETS + "#" + "CMEBiodiesel_001" }
				  };
		} else if (choicePlant.equals("Methanol")) {
			results[0] = downloadZCEMarketData();
			logger.info(results[0]);
			results[1] = downloadHNGMarketData();
			logger.info(results[1]);
			
			addresses = new String[][] {
				{ ONTO_PATH_ELEC_MARKETS + "#" + "data",
				  ONTO_PATH_KB_MARKETS + "#" + "ZCEMethanol_001" },
				{ ONTO_PATH_ELEC_MARKETS + "#" + "data", 
				  ONTO_PATH_KB_MARKETS + "#" + "CMENaturalGas_001" }
			};
		}


		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; URIs of relevant individuals and their
		 * properties are defined and locations of the CSV
		 * files with the market data are stored in KB one
		 * by one
		 */
		
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload()) + "/OntoArbitrage_Market_KB.owl";
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(filePath);

		
		
		
		for (int i = 0; i < addresses.length; i++) {
			if (results[i] == null) {
				continue;
			}
			DatatypeProperty property = jenaOwlModel.getDatatypeProperty(addresses[i][0]);
			Individual individual = jenaOwlModel.getIndividual(addresses[i][1]);
			
			individual.setPropertyValue(property, jenaOwlModel.createTypedLiteral(results[i]));
		}
		
		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */
		savefile(jenaOwlModel, filePath);	
		Gson g = new Gson();
		return g.toJson(results);
	}

	/**
	 * this function calls cmd to execute a Python script
	 * which downloads exchange rates and print them to the
	 * console; those are captured and stored in JPS
	 * knowledge base; the currencies are defined within the
	 * script; first currency-pair header is returned for
	 * testing purposes
	 * 
	 * @return
	 * @throws Exception
	 */
	public static String downloadingAndSavingExchangeRatesInTheKnowledgeBase()
			throws Exception {

		String currency_download = new String(
				"caresjpsarbitrage/exchange_rates.pyw");

		String result = PythonHelper.callPython(
				currency_download, "whatever",
				new DataDownload());
		logger.info(result);

		/**
		 * split the console output into headers and
		 * exchange rates
		 */
		Gson objGson = new GsonBuilder().setPrettyPrinting().create();
		Type listType = new TypeToken<Map<String, String>>(){}.getType();
		
		Map<String, String> mapHeadersRates = objGson.fromJson(result, listType);
		String[] headers = mapHeadersRates.keySet().toArray(new String[0]);
		String[] rates = mapHeadersRates.values().toArray(new String[0]);

		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */
		String[][] addresses = new String[headers.length][];
		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ONTO_PATH_ONTOCAPE + "#" + "numericalValue",
					ONTO_PATH_KB_UTIL_EXRATES + "#" + "V_" + headers[i] };
			logger.info(addresses[i][1]);
		}

		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; rates are stored in KB one by one
		 */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload()) + "/OntoArbitrage_PlantInfo_KB.owl";
		
		System.out.println("My filepath = " + filePath);
		OntModel jenaOwlModel1 = ModelFactory.createOntologyModel();
		jenaOwlModel1.read(filePath);


		for (int i = 0; i < addresses.length; i++) {

			DatatypeProperty property = jenaOwlModel1.getDatatypeProperty(addresses[i][0]);
			//DatatypeProperty property =jenaOwlModel1.getDatatypeProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");	
			Individual individual = jenaOwlModel1.getIndividual(addresses[i][1]);	
			individual.setPropertyValue(property, jenaOwlModel1.createTypedLiteral(new Double(rates[i])));

		}
		
		
		
		

		
		
		

		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */

		savefile(jenaOwlModel1,filePath);
		logger.info("File saved");

//		return headers[0];
		
		// delete later
//		String[] headersAndRates = Stream.of(headers, rates).flatMap(Stream::of).toArray(String[]::new);
//		return StringUtils.join(headersAndRates, "\r\n");	
//		String[][] headersAndRates = {headers, rates};
//		Gson g = new Gson();
//		String headersAndRatesString = g.toJson(headersAndRates);
//		return headersAndRatesString;
		return result;
	}
	
	/**
	 * this function accepts an array and stored in JPS
	 * knowledge base
	 * 
	 * @throws Exception
	 */
	public static String savingDataInTheKnowledgeBase(String receivedData) throws Exception {
		Gson g = new Gson();
		String[][] arrayHeaderPrices = g.fromJson(receivedData, String[][].class);
		
		/**
		 * split the console output into headers and data
		 */
		String[] headers = arrayHeaderPrices[0];
		String[] data = arrayHeaderPrices[1];

		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */
		String[][] addresses = new String[headers.length][];

		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ONTO_PATH_ONTOCAPE + "#" + "numericalValue",
					ONTO_PATH_KB_UTIL_EXRATES + "#" + headers[i] };
		}

		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; rates are stored in KB one by one
		 */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload()) + "/OntoArbitrage_PlantInfo_KB.owl";	
		OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
		jenaOwlModel2.read(filePath);

		for (int i = 0; i < addresses.length; i++) {
			DatatypeProperty property = jenaOwlModel2.getDatatypeProperty(addresses[i][0]);
			Individual individual = jenaOwlModel2.getIndividual(addresses[i][1]);
			individual.setPropertyValue(property, jenaOwlModel2.createTypedLiteral(data[i]));
		}

		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */

		savefile(jenaOwlModel2,filePath);
		
		System.out.println("result from savingDataInTheKnowledgeBase= "+retrievePrices(headers));
		return retrievePrices(headers);
	}

	/**
	 * this function receives names of individuals, which
	 * are to be found in JPS knowledge base, and retrieves
	 * data under numericalValue associated with them; the names and the
	 * data are converted into a string and returned
	 * 
	 * @param headers
	 * @return
	 * @throws Exception
	 */
	public static String retrievePrices(String[] headers) throws Exception {
		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */

		String[][] addresses = new String[headers.length][];
		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ONTO_PATH_ONTOCAPE + "#" + "numericalValue",
					ONTO_PATH_KB_UTIL_EXRATES + "#" + headers[i] };
			logger.info(addresses[i][1]);
		}
		
	logger.info("My retreive prices function");
		/** get model from an owl file */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload()) + "/OntoArbitrage_PlantInfo_KB.owl";
		OntModel jenaOwlModel3 = ModelFactory.createOntologyModel();
		jenaOwlModel3.read(filePath);
		
		
		Gson objGson = new GsonBuilder().create();
		Map<String, String> mapHeaderName = new HashMap<>();
		

		for (int i = 0; i < addresses.length; i++) {
			Individual individual = jenaOwlModel3.getIndividual(addresses[i][1]);
			DatatypeProperty prop=jenaOwlModel3.getDatatypeProperty(addresses[i][0]);
			String name = individual.getPropertyValue(prop).asLiteral().toString(); 
			mapHeaderName.put(headers[i], name);
		}
		return objGson.toJson(mapHeaderName);
	}

	
	/**
	 * this function receives an entity and returns market price of that entity from knowledge base
	 * @param String entity; one of Crude Palm Oil, Biodiesel, Methanol, Natural Gas
	 * @return String market prices
	 */
	public static String retrieveMarketPricesFromKnowledgeBase(String entity) throws Exception {
		
		String stringIndividual = ONTO_PATH_KB_MARKETS + "#" + entity;
		String stringProperty = ONTO_PATH_ELEC_MARKETS + "#data";

		/** get model from an owl file */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload())+ "/OntoArbitrage_Market_KB.owl";
		
		OntModel jenaOwlModel4 = ModelFactory.createOntologyModel();
		jenaOwlModel4.read(filePath);

		Individual individual = jenaOwlModel4.getIndividual(stringIndividual);
		//String name = individual.getPropertyValueLiteral(owlModel.getRDFProperty(stringProperty)).getString();
		String name = individual.getPropertyValue(jenaOwlModel4.getProperty(stringProperty)).asLiteral().getString();
		return name;
	}
	
	public static String retrieveCPOMarketPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("CMECrudePalmOil_001");		
	}
	
	public static String retrieveBiodieselPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("CMEBiodiesel_001");
	}
	
	public static String retrieveMethanolPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("ZCEMethanol_001");
	}
	
	public static String retrieveNaturalGasPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("CMENaturalGas_001");
	}
	
	
	
	/**
	 * this function receives names of individuals, which
	 * are to be found in JPS knowledge base, and retrieves
	 * data under numericalValue associated with them; in
	 * addition, market prices for crude palm oil (CPO) and
	 * biodiesel (FAME) are retrieved; the names and the
	 * data are converted into a string and returned
	 * 
	 * @param headers
	 * @return
	 * @throws Exception
	 */
	public static String retrievingUtilityPricesByProvidingTheirLocationsAndCPOAndFAMEMarketPricesFromTheKnowledgeBase(
			String[] headers) throws Exception {
		
		Gson g = new Gson();
		// return as a json-serialized string of a 1d array of length 3
		String[] data = {
				retrievePrices(headers),
				retrieveCPOMarketPricesFromKnowledgeBase(),
				retrieveBiodieselPricesFromKnowledgeBase()
		};
		return g.toJson(data);
	}
	
	public static String retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase(
			String[] headers) throws Exception {
		
		Gson g = new Gson();
				
		// return as a json-serialized string of a 1d array of length 3
		String[] data = {
				retrievePrices(headers),
				retrieveNaturalGasPricesFromKnowledgeBase(),
				retrieveMethanolPricesFromKnowledgeBase()
		};
		
		return g.toJson(data);
	}	
}
