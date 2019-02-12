package uk.ac.cam.cares.jps.thermo.servlet;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;

import org.json.JSONObject;



import uk.ac.cam.cares.jos.thermo.json.parser.JsonToJsonConverter;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.thermo.calculation.ThermoCalculation;
import uk.ac.cam.cares.jps.thermo.manager.SPARQLManager;

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

@WebServlet("/tc")
public class CompChemRdf4JServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	public static Logger logger = Logger.getLogger(CompChemRdf4JServlet.class.getName());

	public static String catalinaFolderPath = System.getProperty("catalina.home");

	String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";

	/**
	 * @author NK510 Root folder inside Apache Tomcat.
	 */
	public static final String RESULT_FOLDER = catalinaFolderPath + "/webapps/ROOT/";

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
	
			throws ServletException, IOException {
		
		response.setContentType("text/html;charset=UTF-8");
		
		Set<String> jsonSet = new HashSet<String>();
		
		String folderName = "";

		/**
		 * 
		 * Reads input parameter given as IRI.
		 * 
		 */

		JSONObject parameterOne = AgentCaller.readJsonParameter(request);

		String gaussian = parameterOne.getString("gaussian");

		/**
		 * Folder that is already created on server side (Apache Tomcat) where result of
		 * sparql query and result of thermo calcuation will be saved.
		 */
		folderName = gaussian.substring(gaussian.lastIndexOf("/") + 1);

		System.out.println("CompChem IRI: " + gaussian + "  folder name: " + folderName);

		/**
		 * @author NK510 Name of Json file that contains results of thermo calculations.
		 * 
		 */
		String jsonSPARQLOutputFilePath = RESULT_FOLDER + folderName + "/" + folderName + ".json";

		/**
		 * @author NK510 Querying CompChem remote RDF4J repository.
		 */
		SPARQLManager sparqlManager = new SPARQLManager();

		sparqlManager.runCompChemSPARQL(gaussian, jsonSPARQLOutputFilePath, serverUrl);	

		/**
		 * 
		 * @author NK510 Thermo calculation run by Python script.
		 * 
		 */

		ThermoCalculation thermoCalculation = new ThermoCalculation();

		thermoCalculation.runThermoCalculation(jsonSPARQLOutputFilePath, catalinaFolderPath);
		
		JsonToJsonConverter jsonConverter = new JsonToJsonConverter();
		
		/**
		 * 
		 * @author NK510
		 * Updates generated json file with two features: "uniqueSpeciesIRI" and "quantumCalculationIRI":
		 * 
		 */
		String jsonOutputFilePath = RESULT_FOLDER + folderName + "/" + folderName +"_nasa"+ ".json";
		
		jsonSet = jsonConverter.getListIRI(jsonSPARQLOutputFilePath);
		
		logger.info("jsonList.size(): " + jsonSet.size());
        
		List<String> jsonList = new ArrayList<String>(jsonSet);
		
		
		logger.info("jsonList.get(0): " + jsonList.get(0));
		logger.info("jsonList.get(1): " + jsonList.get(1));
		logger.info("jsonList.get(2): " + jsonList.get(2));
		
		/**
		 * @author NK510
		 * Converts updated json object into String.
		 */
		String updatedJsonContent = jsonConverter.updateJsonContent(jsonOutputFilePath, jsonList.get(1), jsonList.get(2), jsonList.get(0));
		
		
		/**
		 * @author NK510
		 * File path where updated json content will be saved.
		 */
		String updatedJsonOutputFilePath = RESULT_FOLDER + folderName + "/" + folderName +"_updated_nasa"+ ".json";
		
		jsonConverter.writeUpdatedJsonToFile(updatedJsonContent, updatedJsonOutputFilePath, response);	
		
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		super.doPost(request, response);
		
	}	
}