package uk.ac.cam.cares.jps.market;

import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFProperty;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.PythonHelper;

import com.google.gson.Gson;

public class DataDownload {
	private static Logger logger = LoggerFactory
			.getLogger(DataDownload.class);

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

		String CPO_download = new String(
				"caresjpsarbitrage/CPO_download.pyw");
		String FAME_download = new String(
				"caresjpsarbitrage/FAME_download.pyw");
		String ZCE_download = new String(
				"caresjpsarbitrage/ZCE_download.pyw");
		String HNG_download = new String(
				"caresjpsarbitrage/HNG_download.pyw");

		String CPO_page = new String(
				"http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/usd-malaysian-crude-palm-oil-calendar.html?optionProductId=8075");
		String FAME_page = new String(
				"http://www.cmegroup.com/trading/energy/refined-products/fame-0-argus-biodiesel-fob-rdam-red-compliant-swap-futures.html");
		String ZCE_page = new String(
				"http://english.czce.com.cn/enportal/DFSStaticFiles/Future/EnglishFutureQuotesMA.htm");
		String HNG_page = new String(
				"http://www.cmegroup.com/trading/energy/natural-gas/natural-gas.html");

		String[][] commands = { { CPO_download, CPO_page },
				{ FAME_download, FAME_page },
				{ ZCE_download, ZCE_page },
				{ HNG_download, HNG_page } };

		String[] results = new String[commands.length];
		for (int i = 0; i < commands.length; i++) {
			String result = PythonHelper.callPython(
					commands[i][0], commands[i][1],
					new DataDownload());
			results[i] = result;
			logger.info(result);
		}

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

		// return results[0];
		// maybe return array of Strings?

		// delete later
		// returns single String of joined elements in an
		// Array
		return StringUtils.join(results, "\r\n");
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
		int results_size = result.split(",").length;
		String[] headers = Arrays.copyOfRange(
				result.split(","), 0, results_size / 2);
		String[] rates = Arrays.copyOfRange(
				result.split(","), results_size / 2,
				results_size);

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
		logger.info("File saved with " + errors.size()
				+ " errors.");

		// return headers[0];

		// delete later
		// String[] headersAndRates = Stream.of(headers,
		// rates).flatMap(Stream::of).toArray(String[]::new);
		// return StringUtils.join(headersAndRates, "\r\n");

		String[][] headersAndRates = { headers, rates };

		Gson g = new Gson();
		String headersAndRatesString = g
				.toJson(headersAndRates);

		return headersAndRatesString;
	}

	/**
	 * this function calls cmd to execute a Python script
	 * which prints input and output headers and the
	 * associated data from an Aspen model; those are
	 * captured and stored in JPS knowledge base;
	 * information to be sourced from the model and printed
	 * is defined in the script
	 * 
	 * this function is more or less obsolete, but was
	 * retained in case anyone would like to use Aspen as a
	 * source of info about a chemical plant
	 * 
	 * @throws Exception
	 */
	@SuppressWarnings("unused")
	private static void retrievingPlantDataFromAspenModelAndSavingItInTheKnowledgeBase()
			throws Exception {

		String Aspen_data = new String(
				"caresjpsarbitrage/print_Aspen_data.pyw");

		String result = PythonHelper.callPython(Aspen_data,
				"1", new DataDownload());
		logger.info(result);

		/**
		 * split the console output into headers and
		 * exchange rates
		 */
		int results_size = result.split(",").length;
		String[] headers = Arrays.copyOfRange(
				result.split(","), 0, results_size / 2);
		String[] data = Arrays.copyOfRange(
				result.split(","), results_size / 2,
				results_size);

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

		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; // KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";

		String data = "";

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

		for (int i = 0; i < addresses.length; i++) {
			RDFIndividual individual = owlModel
					.getRDFIndividual(addresses[i][1]);
			String name = individual
					.getPropertyValueLiteral(
							owlModel.getRDFProperty(
									addresses[i][0]))
					.getString();
			data += headers[i] + ",";
			data += name + ",";
		}

		/** ontology addresses */
		String ontoPath3 = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath4 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";

		String[][] addresses2 = {
				{ ontoPath3 + "#" + "data",
						ontoPath4 + "#"
								+ "CMECrudePalmOil_001" },
				{ ontoPath3 + "#" + "data", ontoPath4 + "#"
						+ "CMEBiodiesel_001" },
				// {ontoPath3+"#"+"data",
				// ontoPath4+"#"+"ZCEMethanol_001"},
				// {ontoPath3+"#"+"data",
				// ontoPath4+"#"+"CMENaturalGas_001"}
		};

		/** get model from an owl file */
		String filePath2 = AgentLocator
				.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_Market_KB.owl";
		OWLModel owlModel2 = null;

		try {
			owlModel2 = ProtegeOWL
					.createJenaOWLModelFromURI(
							"file:/" + filePath2);
		} catch (OntologyLoadException e1) {
			logger.warn(e1.getMessage());
		}

		for (int i = 0; i < addresses2.length; i++) {
			RDFIndividual individual = owlModel2
					.getRDFIndividual(addresses2[i][1]);
			String name = individual
					.getPropertyValueLiteral(
							owlModel2.getRDFProperty(
									addresses2[i][0]))
					.getString();
			data += name + ",";
		}
		System.out.println(1);
System.out.println(data);
System.out.println(1);
		return data;
	}

	/**
	 * this function receives names of individuals, which
	 * are to be found in JPS knowledge base, and retrieves
	 * data under numericalValue associated with them; in
	 * addition, market prices for natural gas (HNG) and
	 * methanol (ZCE) are retrieved; the names and the data
	 * are converted into a string and returned
	 * 
	 * @param headers
	 * @return
	 * @throws Exception
	 */
	public static String retrievingUtilityPricesByProvidingTheirLocationsAndHNGAndZCEMarketPricesFromTheKnowledgeBase(
			String[] headers) throws Exception {

		/**
		 * URIs of ontologies used to define KBs in which
		 * market data will be stored
		 */
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; // KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";

		String data = "";

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

		for (int i = 0; i < addresses.length; i++) {
			RDFIndividual individual = owlModel
					.getRDFIndividual(addresses[i][1]);
			String name = individual
					.getPropertyValueLiteral(
							owlModel.getRDFProperty(
									addresses[i][0]))
					.getString();
			data += headers[i] + ",";
			data += name + ",";
		}

		/** ontology addresses */
		String ontoPath3 = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath4 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";

		String[][] addresses2 = {
				{ ontoPath3 + "#" + "data",
						ontoPath4 + "#"
								+ "CMENaturalGas_001" },
				{ ontoPath3 + "#" + "data", ontoPath4 + "#"
						+ "ZCEMethanol_001" }, };

		/** get model from an owl file */
		String filePath2 = AgentLocator
				.getPathToWorkingDir(new DataDownload())
				+ "/OntoArbitrage_Market_KB.owl";
		OWLModel owlModel2 = null;

		try {
			owlModel2 = ProtegeOWL
					.createJenaOWLModelFromURI(
							"file:/" + filePath2);
		} catch (OntologyLoadException e1) {
			logger.warn(e1.getMessage());
		}

		for (int i = 0; i < addresses2.length; i++) {
			RDFIndividual individual = owlModel2
					.getRDFIndividual(addresses2[i][1]);
			String name = individual
					.getPropertyValueLiteral(
							owlModel2.getRDFProperty(
									addresses2[i][0]))
					.getString();
			data += name + ",";
		}
		//System.out.println(data);
		return data;
	}

}
