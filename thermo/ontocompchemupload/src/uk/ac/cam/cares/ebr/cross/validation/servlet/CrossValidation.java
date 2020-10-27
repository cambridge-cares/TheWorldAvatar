package uk.ac.cam.cares.ebr.cross.validation.servlet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import net.schmizz.sshj.SSHClient;
import net.schmizz.sshj.common.IOUtils;
import net.schmizz.sshj.connection.channel.direct.Session;
import net.schmizz.sshj.transport.verification.PromiscuousVerifier;
import uk.ac.cam.cares.ebr.manager.JsonManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * 
 * Servlet implementation class CrossValidation
 * 
 * Runs Cross validation java code that is stored on HPC machine. Generates json
 * files as output of the calculation for each step in cross validation
 * algorithm. Json files contain species name, enthslpy of formation for each
 * reaction, validity of species, reactions list.
 * 
 * To run the cross validation code on HPC, please go to 
 *          uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.solver.glpk.TerminalGLPKSolver class,
 *         and uncomment " map.put("glpsol", System.getProperty("user.dir") + "/glpk-4.65/examples/glpsol"); " line in order to allow GLPK solver to work on Windows machine.
 *         and comment the line " map.put("glpsol", System.getProperty("user.dir") + "/glpk/w32/glpsol"); ".
 *
 *
 * 
 */

@WebServlet("/crossValidation")
public class CrossValidation extends HttpServlet {

	private static final long serialVersionUID = 1L;

	/** The Constant logger. */
	public static Logger logger = Logger.getLogger(CrossValidation.class.getName());

	/**
	 * 
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 * 
	 */

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		response.setContentType("text/html;charset=UTF-8");

		BasicConfigurator.configure();

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk) Reads input parameter and stores it
		 *         in JSONObject. The input parameter refers to json file that contains
		 *         folder path to Gaussian files (reference species) and folder path for
		 *         target species (csv file). In this version of the web service, both
		 *         folders should be store on server side (HPC machine).
		 *         
		 *         Json file "species.json" is located in ROOT folder of Apache Tomcat server. For example: C:\apache-tomcat-8.5.35\webapps\ROOT\data\species\species.json
		 * 
		 */

		JSONObject parameterOne = AgentCaller.readJsonParameter(request);

		String speciesJson = parameterOne.getString("jsonfile");

		JSONObject jsonFile = JsonManager.readsJsonFileFromUrl(speciesJson);

		logger.info("jsonFile- reference species: " + jsonFile.get("ReferenceSpecies"));

		logger.info("jsonFile- target species: " + jsonFile.get("TargetSpecies"));

		SSHClient ssh = new SSHClient();

		try {

			ssh.addHostKeyVerifier(new PromiscuousVerifier());

			ssh.loadKnownHosts();

			/**
			 * 
			 * Connection to the HPC
			 * 
			 */

			ssh.connect("172.25.186.150");

			ssh.authPassword("nkrd01", "..Nk4c19..");

			final Session session = ssh.startSession();

			try {

				/**
				 * 
				 * Remote execution simple Java code as jar file on remote HPC cluster.
				 * 
				 */

//			final Session.Command cmd_ssh = session.exec("ssh cn01; java -cp /home/nkrd01/jartest_1.jar uk.ac.ceb.cam.Printing "+ " " + jsonFile.get("ReferenceSpecies") + " " + jsonFile.get("TargetSpecies"));

				/**
				 * 
				 * Run Cross validation (Philipp's) code on HPC by using ssh execution.
				 * 
				 */

				/**
				 * 
				 * Creates a unique folder on HPC where all results of cross validation
				 * calculation will be saved. Runs Cross validation java implementation (jar
				 * file) stored on the HPC machine with input parameters.
				 * 
				 */

				String dirName = generateUniqueFolderName("crossvalidation");

				/**
				 * 
				 * Input parameters: folder where Gaussian files are stored, folder path where
				 * csv file is stored, folder path where files used by GLPK solver are used
				 * (.temp folder), and folder path where all results of cross validation
				 * calculation will be saved.
				 * 
				 */

				final Session.Command cmd_ssh = session.exec("mkdir /home/nkrd01/" + dirName + "; mkdir /home/nkrd01/"+ dirName + "/" + "LeaveOneOutCrossValidation_temp" + "; ssh cn01; java -jar /home/nkrd01/cv_isg_ti_r5_args_v1.jar " + " "+ jsonFile.get("ReferenceSpecies") + " " + jsonFile.get("TargetSpecies") + " " + dirName + " "+ "/home/nkrd01/" + dirName + "/" + "LeaveOneOutCrossValidation_temp");

				logger.info("Printing message from HPC: " + IOUtils.readFully(cmd_ssh.getInputStream()).toString() + " " + jsonFile.get("ReferenceSpecies") + " " + jsonFile.get("TargetSpecies") + " " + dirName);

				response.getWriter().append("exit status: " + cmd_ssh.getExitStatus());

				response.getWriter().append("jsonFile- reference species: " + jsonFile.get("ReferenceSpecies"));

				response.getWriter().append("Served at: ").append(request.getContextPath() + ".");

				response.getWriter().append("species json: " + speciesJson);

				logger.info("species json: " + speciesJson);

				cmd_ssh.join(10, TimeUnit.SECONDS);

			} finally {

				session.close();

			}

		} finally {

			ssh.disconnect();

			ssh.close();

		}
	}

	/**
	 * 
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 * 
	 */

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		doGet(request, response);

	}	

	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param fileName a String that represents file name.
	 * @return String as a unique folder name.
	 * @throws UnsupportedEncodingException the exception.
	 * 
	 */
	public static String generateUniqueFolderName(String fileName) throws UnsupportedEncodingException {

		long milliseconds = System.currentTimeMillis();

		String datetime = new Date().toString();

		datetime = datetime.replace(" ", "");
		datetime = datetime.replace(":", "");

		/**
		 * 
		 * @author nk510 (caresssd@hermes.cam.ac.uk)
		 * 
		 *         Generates source for universally unique identifier (uuid) based on
		 *         file name, date, time, and cpu milliseconds.
		 * 
		 */

		String source = fileName + datetime + milliseconds;

		byte[] bytes = source.getBytes("UTF-8");

		UUID uuid = UUID.nameUUIDFromBytes(bytes);

		return uuid.toString();

	}

}