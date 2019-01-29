package uk.ac.cam.cares.jps.thermo.servlet;

import java.io.File;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.util.FileManager;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

import uk.ac.cam.cares.jps.thermo.manager.FolderManager;

import uk.ac.cam.ceb.como.jaxb.parsing.utils.FileUtility;
import uk.ac.cam.ceb.como.jaxb.parsing.utils.Utility;

/**
 *
 *@author NK510
 * Servlet implementation class ServiceTemplate Takes an input parameter as IRI
 * (owl file) and runs thermo calculations. Results of thermo calculations are
 * stored in json file.
 * 
 */

@WebServlet("/calculation")
public class CompChemOWLFileServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	final static Logger logger = Logger.getLogger(CompChemOWLFileServlet.class.getName());

	public static String catalinaFolderPath = System.getProperty("catalina.home");

	public static final String SPARQL_FOLDER = catalinaFolderPath + "/conf/Catalina/sparql_query/";

	public static final String RESULT_FOLDER = catalinaFolderPath + "/webapps/ROOT/temp/JPS_THERMO/";

	private String folderName = "";

	/**
	 * 
	 * @see HttpServlet#HttpServlet()
	 * 
	 */

	public CompChemOWLFileServlet() {

		super();

		// TODO Auto-generated constructor stub
	}

	/**
	 * 
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 * 
	 */

	@Override
	synchronized protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

//		super.doGet(request, response);
		
		JSONObject parameterOne = AgentCaller.readJsonParameter(request);

		String gaussian = parameterOne.getString("gaussian");

		logger.info("INPUT FOR THERMO: gaussian = " + gaussian);

		Utility utility = new FileUtility();

		String fileName = gaussian.substring(gaussian.lastIndexOf("/") + 1);

		folderName = FolderManager.generateUniqueFolderName(fileName, catalinaFolderPath);

		FolderManager.createFolder(RESULT_FOLDER + folderName);

		OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM_TRANS_INF);

		FileManager.get().readModel(model, gaussian);

		/**
		 * @author NK510
		 * Runs SPARQL query that returns data that are input for thermo calculation.
		 * Results of SPARQL query are stored in json file.
		 */
		
		File sparqlFile = new File(SPARQL_FOLDER + "query_all.sparql");

		String q = FileUtils.readFileToString(sparqlFile, "UTF-8");

		Query query = QueryFactory.create(q);

		QueryExecution qexec = QueryExecutionFactory.create(query, model);

		FileOutputStream fileOutputStream = null;

		String jsonInputFilePath = RESULT_FOLDER + folderName + "/" + StringUtils.substringBefore(fileName, ".") + ".json";

		try {

			ResultSet resultSet = qexec.execSelect();

			while (resultSet.hasNext()) {

				fileOutputStream = new FileOutputStream(new File(jsonInputFilePath), true);

				ResultSetFormatter.outputAsJSON(fileOutputStream, resultSet);

			}

		} finally {

			qexec.close();

			fileOutputStream.close();
		}

		List<File> jsonInputFileList = utility.getArrayFileList(RESULT_FOLDER + folderName + "/", ".json");

		/**
		 * @author NK510
		 * Runs thermo-calculation Python script over data queried by SPARQL.
		 * 
		 */
		for (int i = 0; i < jsonInputFileList.size(); i++) {

			String pyscript = catalinaFolderPath + "/conf/Catalina/c4e-dln22-TDC/Source/thermoDriver.py";

			String[] cmd = { "python", pyscript, "-j", jsonInputFileList.get(i).getAbsolutePath(), };

			Runtime.getRuntime().exec(cmd);
		}

		List<File> jsonOutputFileList = utility.getArrayFileList(RESULT_FOLDER + folderName + "/", ".json");

		System.out.println("jsonOutputFileList.size(): " + jsonOutputFileList.size());

		String content = new QueryBroker().readFile(jsonInputFilePath);

		response.getWriter().write(content);

	}

	/**
	 * 
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 * 
	 */

	@Override
	synchronized protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

	public void getThermoCalculationAgent() {

		JSONObject json = new JSONObject();

		json.put("gaussian", "http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");

		// GET ...twa.com/JPS_THERMO/thermocalcualtion?query={"gaussian":".......owl"}

		String result = AgentCaller.executeGetWithJsonParameter("JPS_THERMO/thermocalculation", json.toString());

		System.out.println("result = " + result);
	}
}