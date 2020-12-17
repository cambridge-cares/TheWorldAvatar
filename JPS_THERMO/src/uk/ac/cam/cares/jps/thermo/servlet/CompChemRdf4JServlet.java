package uk.ac.cam.cares.jps.thermo.servlet;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.thermo.calculation.ThermoCalculation;
import uk.ac.cam.cares.jps.thermo.json.parser.JsonToJsonConverter;
import uk.ac.cam.cares.jps.thermo.json.parser.JsonToOwlConverter;
import uk.ac.cam.cares.jps.thermo.manager.FolderManager;
import uk.ac.cam.cares.jps.thermo.manager.PropertiesManager;
import uk.ac.cam.cares.jps.thermo.manager.SPARQLManager;
import uk.ac.cam.cares.jps.thermo.manager.UploadOntology;

/**
 * 
 * @author NK510
 * 
 *         This servlet does the following:
 *         <ul>
 *         <li>Queries CompChem remote repository (RDF4J), saves results of that
 *         query as Json file.</li>
 *         <li>Run thermo calculations and generates json file that contains
 *         results of that calculation.</li>
 *         <li>Implementation is not thread safe. Not tested whether it is
 *         therad safe or no. Some implemented methods are synchronized.</li>
 *         </ul>
 *
 */

@WebServlet("/calculation")
public class CompChemRdf4JServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	public static Logger logger = Logger.getLogger(CompChemRdf4JServlet.class.getName());

	private Properties jpsThermoProperties = PropertiesManager.loadProperties(
			CompChemRdf4JServlet.class.getClassLoader().getResourceAsStream("jps_thermo.management.properties"));

	private String compchemServerUrl = jpsThermoProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url");

	private String ontokinServerUrl = jpsThermoProperties.getProperty("ontokin.kb.local.rdf4j.server.url");

	private String ontospeciesServerUrl = jpsThermoProperties.getProperty("ontospecies.kb.local.rdf4j.server.url");

	private String aboxOntokinUri = jpsThermoProperties.getProperty("abox.ontokin.uri");

	private final String RESULT_ONTOCOMPCHEM_FOLDER = jpsThermoProperties.getProperty("result.ontocompchem.folder");

	private final String RESULT_ONTOKIN_FOLDER = jpsThermoProperties.getProperty("result.ontokin.folder");

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)

			throws ServletException, IOException {

		response.setContentType("text/html;charset=UTF-8");

		List<String> jsonList = new ArrayList<String>();

		String folderName = "";

		/**
		 * 
		 * Reads input parameter given as IRI.
		 * 
		 */

		JSONObject parameterOne = AgentCaller.readJsonParameter(request);

		String gaussian = parameterOne.getString("gaussian");

		logger.info("gaussian: " + gaussian);

		/**
		 * 
		 * Folder that is already created on server side (Apache Tomcat) where result of
		 * sparql query and result of thermochemistry calculation will be saved.
		 * 
		 */
		folderName = gaussian.substring(gaussian.lastIndexOf("#") + 1);

		logger.info("folder name:  " + folderName);

		logger.info("OntoCompChem IRI: " + gaussian + "  folder name: " + folderName);

		/**
		 * 
		 * @author NK510 Name of Json file that contains results of thermo calculations.
		 * 
		 */
		String jsonSPARQLOutputFilePath = RESULT_ONTOCOMPCHEM_FOLDER + folderName + "/" + folderName + ".json";

		String jsonOutputFilePath = RESULT_ONTOCOMPCHEM_FOLDER + folderName + "/" + folderName + "_nasa" + ".json";

		String updatedJsonOutputFilePath = RESULT_ONTOCOMPCHEM_FOLDER + folderName + "/" + folderName + "_updated_nasa"
				+ ".json";

		/**
		 * 
		 * @author NK510 Querying 'ontocompchem' remote RDF4J repository. Result of the
		 *         this query is stored as JSON file.
		 * 
		 */
		SPARQLManager sparqlCompChemlManager = new SPARQLManager();

		sparqlCompChemlManager.runCompChemSPARQL(gaussian, jsonSPARQLOutputFilePath, compchemServerUrl);

		/**
		 * 
		 * @author NK510
		 * 
		 *         Returns species URI for given gaussian URI
		 * 
		 */

		SPARQLManager sparqlSpeciesManager = new SPARQLManager();
		String speciesUri = sparqlSpeciesManager.getUniqueSpeciesUri(compchemServerUrl, gaussian);

		logger.info("species-Uri inside CompChemRdf :  " + speciesUri);

		/**
		 * 
		 * @author NK510
		 * 
		 *         SPARQL query performed on 'ontospecies' graph. Returns enthalpy of
		 *         formation and temperature for given species id.
		 * 
		 */

		SPARQLManager sparqEnthalpyManager = new SPARQLManager();
		
		String enthalpyOfFormationAndTemperature = sparqEnthalpyManager.getEnthalpyOfFormation(ontospeciesServerUrl, speciesUri);

		logger.info("enthalpyOfFormation (CompChemRef) : " + enthalpyOfFormationAndTemperature);

		/**
		 * 
		 * @author NK510 Thermo calculation run by Python script.
		 * 
		 */

		ThermoCalculation thermoCalculation = new ThermoCalculation();

		thermoCalculation.runThermoCalculation(jsonSPARQLOutputFilePath, jsonOutputFilePath, enthalpyOfFormationAndTemperature);

		JsonToJsonConverter jsonConverter = new JsonToJsonConverter();

		jsonList.addAll(jsonConverter.getListIRI(jsonSPARQLOutputFilePath));

		logger.info("jsonSet.size(): " + jsonList.size());

		/**
		 * 
		 * @author NK510 Waits 2 second to complete thermo calculation.
		 * 
		 */

		try {

			Thread.sleep(2000);

		} catch (InterruptedException e) {

			e.printStackTrace();
		}

		/**
		 * 
		 * @author NK510 updates json file and converts it into String.
		 * 
		 */

		logger.info("jsonList.get(2): " + jsonList.get(0));
		logger.info("jsonList.get(0): " + jsonList.get(1));
		logger.info("jsonList.get(1): " + jsonList.get(2));

		String updatedJsonContent = jsonConverter.updateJsonContent(jsonOutputFilePath, jsonList.get(0),
				jsonList.get(1), jsonList.get(2));

		/**
		 * 
		 * @author NK510 Updated json is saved to json file.
		 * 
		 */

		jsonConverter.writeUpdatedJsonToFile(updatedJsonContent, updatedJsonOutputFilePath, response);

		/**
		 * 
		 * @author NK510 Waits 2 second to complete thermo calculation.
		 * 
		 */
		try {

			Thread.sleep(2000);

		} catch (InterruptedException e) {

			e.printStackTrace();
		}

		/**
		 * 
		 * @author NK510 Converts generated json file as a result of thermochemistry
		 *         calculations into owl file.
		 * 
		 */

		JsonToOwlConverter jsonToOwlConverter = new JsonToOwlConverter();

		jsonToOwlConverter.convertJsonIntoOwl(updatedJsonOutputFilePath, RESULT_ONTOKIN_FOLDER + folderName + "/");

		/**
		 * 
		 * @author NK Waits 2 second to complete thermo calculation.
		 * 
		 */
		try {

			Thread.sleep(2000);

		} catch (InterruptedException e) {

			e.printStackTrace();
		}
		

		/**
		 * 
		 * @author NK510 Uploads generated Ontokin individual assertions (owl file) into
		 *         RDF4J repository.
		 * 
		 */

		UploadOntology uploadOntology = new UploadOntology();

		uploadOntology.uploadOntoKin(new FolderManager().getOwlFilePath(RESULT_ONTOKIN_FOLDER + folderName + "/"),ontokinServerUrl, aboxOntokinUri);

		FolderManager folderManager = new FolderManager();

		String owlFileName = new FolderManager().getOwlFile(RESULT_ONTOKIN_FOLDER + folderName + "/").getName();

		File sourceFile = new File(new FolderManager().getOwlFile(RESULT_ONTOKIN_FOLDER + folderName + "/").getAbsolutePath());

		File desinationFile = new File(RESULT_ONTOKIN_FOLDER + owlFileName);

		logger.info("sourceFile: " + sourceFile.getAbsolutePath() + "file exists: " + sourceFile.exists());
		
		logger.info("desinationFile: " + desinationFile.getAbsolutePath());		

		folderManager.copyFileToAnotherDestination(sourceFile, desinationFile);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		super.doPost(request, response);

	}
}