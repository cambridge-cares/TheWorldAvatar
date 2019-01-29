package uk.ac.cam.cares.jps.thermo.servlet;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;


import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.thermo.calculation.ThermoCalculation;
import uk.ac.cam.cares.jps.thermo.manager.SPARQLManager;
/**
 * 
 * @author NK510
 * 
 * This servlet does the following:
 * 1. Queries CompChem remote repository (RDF4J), saves results of that query as Json file.
 * 2. Run thermo calculations and generates json file that contains results of that calculation.
 * 3. Implementation is not thread safe.
 *
 */

@WebServlet("/compchemcalculation")
public class CompChemRdf4JServlet extends HttpServlet  {

	private static final long serialVersionUID = 1L;
	
	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(CompChemRdf4JServlet.class.getName());
	
	public static String catalinaFolderPath = System.getProperty("catalina.home");
	
	String serverUrl = "http://localhost:8080/rdf4j-server/repositories/compchemkb";
	
	/**
	 * @author NK510
	 * Root folder inside Apache Tomcat.
	 */
	public static final String RESULT_FOLDER = catalinaFolderPath + "/webapps/ROOT/";
	
	private String folderName = "";
	
	@Override
	synchronized protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		/**
		 * 
		 * Reads input parameter given as IRI.
		 * 
		 */
		
		JSONObject parameterOne = AgentCaller.readJsonParameter(request);

		String gaussian = parameterOne.getString("gaussian");
		
		
		/**
		 * Folder that is already created on server side  (Apache Tomcat) where result of sparql query and result of thermo calcuation will be saved.	
		 */
		folderName = gaussian.substring(gaussian.lastIndexOf("/") + 1);
		
		System.out.println("CompChem IRI: " + gaussian+ "  folder name: " + folderName);
		
		/**
		 * @author NK510
		 * Name of Json file that contains results of thermo calculations.
		 * 
		 */
		String jsonInputFilePath = RESULT_FOLDER + folderName + "/" + folderName +".json";		

		/**
		 * @author NK510
		 * Querying CompChem remote RDF4J repository. 
		 */
		SPARQLManager sparqlManager = new SPARQLManager();
		
		sparqlManager.runCompChemSPARQL(gaussian, jsonInputFilePath, serverUrl);;
		
		/**
		 * @author NK510
		 * Thermo calculation that runs Python script
		 * 
		 */
		 
		ThermoCalculation thermoCalculation = new ThermoCalculation();
		
		thermoCalculation.runThermoCalculation(jsonInputFilePath, catalinaFolderPath);		
				
	}

	@Override
	synchronized protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		super.doPost(request, response);
	
	}
	
	

	
}
