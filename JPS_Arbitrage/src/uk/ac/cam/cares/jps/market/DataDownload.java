package uk.ac.cam.cares.jps.market;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFProperty;
import javafx.beans.value.WritableStringValue;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

import java.lang.reflect.Type;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

public class DataDownload {
	private static Logger logger = LoggerFactory
			.getLogger(DataDownload.class);
	
	public static void writeStringUsingBufferedWriter(String
		 function, String result) throws IOException {
	 BufferedWriter writer = new BufferedWriter(new FileWriter("C:\\jps\\jps_arbitrage\\consoleOutputDataDownload.txt", true));
	 writer.append(function);
	 writer.newLine();
	 writer.append(result);
	 writer.newLine();
	 writer.close();
	}
	
	public static String downloadMarketData(String script, String source) throws Exception {
		return PythonHelper.callPython(script, source, new DataDownload());
	}
	
	public static String downloadCPOMarketData() throws Exception {
		String CPO_download = new String(
				"caresjpsarbitrage/CPO_download.pyw");
		
		String CPO_page = new String(
				"http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075");
		
		return downloadMarketData(CPO_download, CPO_page);
	}
	
	public static String downloadFAMEMarketData() throws Exception {
		String FAME_download = new String(
				"caresjpsarbitrage/FAME_download.pyw");
		
		String FAME_page = new String(
				"http://www.cmegroup.com/trading/energy/refined-products/fame-0-argus-biodiesel-fob-rdam-red-compliant-swap-futures.html");
		
		return downloadMarketData(FAME_download, FAME_page);
	}
	
	public static String downloadZCEMarketData() throws Exception {
		String ZCE_download = new String(
				"caresjpsarbitrage/ZCE_download.pyw");
		
		String ZCE_page = new String(
				"http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm");
		
		return downloadMarketData(ZCE_download, ZCE_page);		
	}
	
	public static String downloadHNGMarketData() throws Exception {
		String HNG_download = new String(
				"caresjpsarbitrage/HNG_download.pyw");
		
		String HNG_page = new String(
				"http://www.cmegroup.com/trading/energy/natural-gas/natural-gas.html");
		
		return downloadMarketData(HNG_download, HNG_page);
	}


	
	/**
	 * this function calls cmd to execute 4 Python scripts
	 * which download market prices for crude palm oil
	 * (CPO), biodiesel (FAME), natural gas at Henry Hub
	 * (HNG) and methanol at Zhengzhou exchange (ZCE) and
	 * stores it JPS knowledge base; prices of CPO are
	 * captured from cmd and returned for testing purposes
	 * 
	 * @return
	 * @throws Exception
	 */
	public static String downloadingAndSavingMarketDataInTheKnowledgeBase()
			throws Exception {

		String[] results = new String[4];
		
		results[0] = downloadCPOMarketData();
		logger.info(results[0]);
		results[1] = downloadFAMEMarketData();
		logger.info(results[1]);
		results[2] = downloadZCEMarketData();
		logger.info(results[2]);
		results[3] = downloadHNGMarketData();
		logger.info(results[3]);

		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath2 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";

		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; URIs of relevant individuals and their
		 * properties are defined and locations of the CSV
		 * files with the market data are stored in KB one
		 * by one
		 */
		String filePath = AgentLocator
				.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_Market_KB.owl";
		FileInputStream inFile = new FileInputStream(
				filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL
				.createJenaOWLModelFromReader(in);

		String[][] addresses = {
				{ ontoPath + "#" + "data",
						ontoPath2 + "#"
								+ "CMECrudePalmOil_001" },
				{ ontoPath + "#" + "data",
						ontoPath2 + "#"
								+ "CMEBiodiesel_001" },
				{ ontoPath + "#" + "data",
						ontoPath2 + "#"
								+ "ZCEMethanol_001" },
				{ ontoPath + "#" + "data", ontoPath2 + "#"
						+ "CMENaturalGas_001" } };

		for (int i = 0; i < addresses.length; i++) {
			if (results[i] == null) {
				continue;
			}
			RDFProperty property = jenaOwlModel
					.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel
					.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property,
					results[i]);
		}

		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */
		Collection<Object> errors = new ArrayList<Object>();
		jenaOwlModel.save(new URI("file:/" + filePath),
				FileUtils.langXMLAbbrev, errors,
				jenaOwlModel.getOntModel());
		logger.info("File saved with " + errors.size()
				+ " errors.");

//		return results[0];
		// maybe return array of Strings?
		
		Gson g = new Gson();
		// delete later
		// returns single String of joined elements in an Array
//		return StringUtils.join(results, "\r\n");
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
		
//		System.out.println(Arrays.toString(headers));
//		System.out.println(Arrays.toString(rates));
		
//		int results_size = result.split(",").length;
//		String[] headers = Arrays.copyOfRange(
//				result.split(","), 0, results_size / 2);
//		String[] rates = Arrays.copyOfRange(
//				result.split(","), results_size / 2,
//				results_size);

		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; // KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";

		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */
		String[][] addresses = new String[headers.length][];
		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ontoPath2 + "#" + "numericalValue",
					ontoPath + "#" + "V_" + headers[i] };
			logger.info(addresses[i][1]);
		}

		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; rates are stored in KB one by one
		 */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_PlantInfo_KB.owl";
		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);

		for (int i = 0; i < addresses.length; i++) {
			RDFProperty property = jenaOwlModel.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property, rates[i]);
		}

		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */
		Collection<Object> errors = new ArrayList<Object>();
		jenaOwlModel.save(new URI("file:/" + filePath),
				FileUtils.langXMLAbbrev, errors,
				jenaOwlModel.getOntModel());
		logger.info("File saved with " + errors.size() + " errors.");

//		return headers[0];
		
		// delete later
//		String[] headersAndRates = Stream.of(headers, rates).flatMap(Stream::of).toArray(String[]::new);
//		return StringUtils.join(headersAndRates, "\r\n");
		
//		String[][] headersAndRates = {headers, rates};
//		
//		Gson g = new Gson();
//		String headersAndRatesString = g.toJson(headersAndRates);
//		
//		return headersAndRatesString;
		return result;
	}
	
	/**
	 * this function accepts an array and stored in JPS
	 * knowledge base; the currencies are defined within the
	 * script; first currency-pair header is returned for
	 * testing purposes
	 * 
	 * @throws Exception
	 */
	public static String savingDataInTheKnowledgeBase(String receivedData) throws Exception {
		Gson g = new Gson();
		String[][] arrayHeaderPrices = g.fromJson(receivedData, String[][].class);
		
		/**
		 * split the console output into headers and
		 * exchange rates
		 */
		String[] headers = arrayHeaderPrices[0];
		String[] data = arrayHeaderPrices[1];

		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; // KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";

		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */
		String[][] addresses = new String[headers.length][];

		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ontoPath2 + "#" + "numericalValue",
					ontoPath + "#" + headers[i] };
		}

		/**
		 * knowledge base from an owl file in a jenaOWL
		 * model; rates are stored in KB one by one
		 */
		String filePath = AgentLocator
				.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_PlantInfo_KB.owl";
		FileInputStream inFile = new FileInputStream(
				filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL
				.createJenaOWLModelFromReader(in);

		for (int i = 0; i < addresses.length; i++) {
			RDFProperty property = jenaOwlModel
					.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel
					.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property, data[i]);
		}

		/**
		 * save the updated model file; also, any error
		 * messages are collected and printed
		 */
		Collection<Object> errors = new ArrayList<Object>();
		jenaOwlModel.save(new URI("file:/" + filePath),
				FileUtils.langXMLAbbrev, errors,
				jenaOwlModel.getOntModel());
		logger.info("File saved with " + errors.size()
				+ " errors.");
		
		String[] inputHeaders = {"V_Price_CoolingWater_001", "V_Price_FuelGas_001", "V_Price_Electricity_001"};
		return retrieveUtilityPrices(inputHeaders);
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
	public static String retrieveUtilityPrices(String[] headers) throws Exception {
		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; // KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";

		/**
		 * URIs of relevant individuals and their properties
		 * are defined
		 */
		String[][] addresses = new String[headers.length][];
		for (int i = 0; i < addresses.length; i++) {
			addresses[i] = new String[] {
					ontoPath2 + "#" + "numericalValue",
					ontoPath + "#" + headers[i] };
			logger.info(addresses[i][1]);
		}

		/** get model from an owl file */
		String filePath = AgentLocator
				.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_PlantInfo_KB.owl";
		OWLModel owlModel = null;

		try {
			owlModel = ProtegeOWL.createJenaOWLModelFromURI(
					"file:/" + filePath);
		} catch (OntologyLoadException e1) {
			logger.warn(e1.getMessage());
		}
		
		Gson objGson = new GsonBuilder().create();
		Map<String, String> mapHeaderName = new HashMap<>();
//		String data = "";
		
		for (int i = 0; i < addresses.length; i++) {
			RDFIndividual individual = owlModel
					.getRDFIndividual(addresses[i][1]);
			
			String name = individual
					.getPropertyValueLiteral(
							owlModel.getRDFProperty(
									addresses[i][0]))
					.getString();
			
			mapHeaderName.put(headers[i], name);
//			data += headers[i] + ",";
//			data += name + ",";
		}
		
//		writeStringUsingBufferedWriter("retrieveUtilityPrices", objGson.toJson(mapHeaderName));
		return objGson.toJson(mapHeaderName);
	}

	
	
	public static String retrieveMarketPricesFromKnowledgeBase(String entity) throws Exception {
		/** ontology addresses */
		String ontoPath3 = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath4 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";
		
		String stringIndividual = ontoPath4 + "#" + entity;
		String stringProperty = ontoPath3 + "#data";

		/** get model from an owl file */
		String filePath = AgentLocator.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_Market_KB.owl";
		OWLModel owlModel = null;

		try {
			owlModel = ProtegeOWL.createJenaOWLModelFromURI(
							"file:/" + filePath);
		} catch (OntologyLoadException e1) {
			logger.warn(e1.getMessage());
		}

		RDFIndividual individual = owlModel.getRDFIndividual(stringIndividual);
		String name = individual.getPropertyValueLiteral(owlModel.getRDFProperty(stringProperty)).getString();
		return name;
	}
	
	public static String retrieveCPOMarketPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("CMECrudePalmOil_001");		
	}
	
	public static String retrieveBiodieselPricesFromKnowledgeBase() throws Exception {
		return retrieveMarketPricesFromKnowledgeBase("CMEBiodiesel_001");
	}
	
	public static String retrieveMethanolPricesFromKnowledgeBase() throws Exception {
		writeStringUsingBufferedWriter("retrieveMethanol", retrieveMarketPricesFromKnowledgeBase("ZCEMethanol_001"));
		return retrieveMarketPricesFromKnowledgeBase("ZCEMethanol_001");
	}
	
	public static String retrieveNaturalGasPricesFromKnowledgeBase() throws Exception {
		writeStringUsingBufferedWriter("retrieveNaturalGas", retrieveMarketPricesFromKnowledgeBase("CMENaturalGas_001"));
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
				retrieveUtilityPrices(headers),
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
				retrieveUtilityPrices(headers),
				retrieveNaturalGasPricesFromKnowledgeBase(),
				retrieveMethanolPricesFromKnowledgeBase()
		};
		return g.toJson(data);
	}
}
