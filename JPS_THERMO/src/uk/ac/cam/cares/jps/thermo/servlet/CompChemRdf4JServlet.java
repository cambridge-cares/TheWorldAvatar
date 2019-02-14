package uk.ac.cam.cares.jps.thermo.servlet;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

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

@WebServlet("/calculation")
public class CompChemRdf4JServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	public static Logger logger = Logger.getLogger(CompChemRdf4JServlet.class.getName());

	public static String catalinaFolderPath = System.getProperty("catalina.home");

	String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";

	/**
	 * @author NK510 Root folder inside Apache Tomcat.
	 */
	public static final String RESULT_FOLDER = catalinaFolderPath + "/webapps/ROOT/kb/";

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

		/**
		 * Folder that is already created on server side (Apache Tomcat) where result of
		 * sparql query and result of thermo calcuation will be saved.
		 */
		folderName = gaussian.substring(gaussian.lastIndexOf("#") + 1);

		logger.info("folder name:  " + folderName);

		System.out.println("CompChem IRI: " + gaussian + "  folder name: " + folderName);

		/**
		 * @author NK510 Name of Json file that contains results of thermo calculations.
		 * 
		 */
		String jsonSPARQLOutputFilePath = RESULT_FOLDER + folderName + "/" + folderName + ".json";


		String jsonOutputFilePath = RESULT_FOLDER + folderName + "/" + folderName + "_nasa" + ".json";

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

		thermoCalculation.runThermoCalculation(jsonSPARQLOutputFilePath, jsonOutputFilePath, catalinaFolderPath);		

		JsonToJsonConverter jsonConverter = new JsonToJsonConverter();

		jsonList.addAll(jsonConverter.getListIRI(jsonSPARQLOutputFilePath));
		
		logger.info("jsonSet.size(): " + jsonList.size());
		
//		List<String> jsonList = new ArrayList<String>(jsonSet);		

		/** 
		 * @author NK
		 * Waits 2 second to complete thermo calculation.
		 * 
		 */
		 try {
			 
			Thread.sleep(2000);
		
		 } catch (InterruptedException e) {
			
			e.printStackTrace();
		}
		 
		/**
		 * @author NK510 updates json file and converts it into String.
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
		
		jsonConverter.writeUpdatedJsonToFile(updatedJsonContent, jsonOutputFilePath, response);

	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		super.doPost(request, response);

	}
}