package uk.ac.cam.cares.jps.agent.gPROMS;

import java.io.File;

import javax.servlet.annotation.WebServlet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Controller;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URLDecoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.commons.io.FileUtils;

import org.json.JSONObject;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentProperty;
//import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
//import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgentException;
import uk.ac.cam.cares.jps.agent.utils.ZipUtility;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.PostProcessing;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * gPROMS Agent developed for setting-up and running gPROMS chemical network on
 * HPC.
 *
 * @author Aravind Devanand (aravind@u.nus.edu)
 *
 */
@Controller
@WebServlet(urlPatterns = { gPROMSAgent.JOB_REQUEST_PATH, gPROMSAgent.JOB_STATISTICS_PATH,
		/* gPROMSAgent.JOB_OUTPUT_REQUEST_PATH */ })
public class gPROMSAgent extends JPSAgent {

	private static final long serialVersionUID = 1L;
	private Logger logger = LoggerFactory.getLogger(gPROMSAgent.class);
	private File workspace;
	static JobSubmission jobSubmission;
	public static ApplicationContext applicationContextgPROMSAgent;
	public static gPROMSAgentProperty gPROMSAgentProperty;

	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to gPROMS Agent";

	public static final String JOB_REQUEST_PATH = "/job/request";
	public static final String JOB_OUTPUT_REQUEST_PATH = "/job/output/request";
	public static final String JOB_STATISTICS_PATH = "/job/statistics";
	public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";

	public JSONObject produceStatistics(String input) throws IOException, gPROMSAgentException {
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		// Initialises all properties required for this agent to set-up<br>
		// and run jobs. It will also initialise the unique instance of<br>
		// Job Submission class.
		initAgentProperty();
		return jobSubmission.getStatistics(input);
	}

	@RequestMapping(value = gPROMSAgent.JOB_SHOW_STATISTICS_PATH, method = RequestMethod.GET)
	@ResponseBody
	public String showStatistics() throws IOException, gPROMSAgentException {
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		initAgentProperty();
		return jobSubmission.getStatistics();
	}

	/**
	 * Starts the asynchronous scheduler to monitor quantum jobs.
	 *
	 * @throws gPROMSAgentException
	 */

	public void init() throws ServletException {
		logger.info("---------- gPROMS Simulation Agent has started ----------");
		System.out.println("---------- gPROMS Simulation Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		gPROMSAgent gPROMSAgent = new gPROMSAgent();
		// initialising classes to read properties from the gPROMS-agent.properites
		// file
		initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				gPROMSAgent.monitorJobs();
			} catch (SlurmJobException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}, gPROMSAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
				gPROMSAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("---------- gPROMS Simulation jobs are being monitored  ----------");
		System.out.println("---------- gPROMS Simulation jobs are being monitored  ----------");

	}

	/**
	 * Initialises the unique instance of the gPROMSAgentProperty class that<br>
	 * reads all properties of gPROMSAgent from the kinetics-agent property
	 * file.<br>
	 *
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the kinetics-agent property file<br>
	 * through the gPROMSAgent class.
	 */
	public void initAgentProperty() {
		// initialising classes to read properties from the kinetics-agent.properites
		// file
		if (applicationContextgPROMSAgent == null) {
			applicationContextgPROMSAgent = new AnnotationConfigApplicationContext(gPROMSAgentConfiguration.class);
		}
		if (gPROMSAgentProperty == null) {
			gPROMSAgentProperty = applicationContextgPROMSAgent.getBean(gPROMSAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(gPROMSAgentProperty.getAgentClass(), gPROMSAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(gPROMSAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty
					.setHpcServerLoginUserPassword(gPROMSAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(gPROMSAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty
					.setAgentCompletedJobsSpacePrefix(gPROMSAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty
					.setAgentFailedJobsSpacePrefix(gPROMSAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(gPROMSAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(gPROMSAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(gPROMSAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(gPROMSAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(gPROMSAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(gPROMSAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(gPROMSAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setJsonFileExtension(gPROMSAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(gPROMSAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(gPROMSAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(
					gPROMSAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty
					.setAgentPeriodicActionInterval(gPROMSAgentProperty.getAgentPeriodicActionInterval());
		}
	}

	/**
	 * Receives and processes HTTP requests that match with the URL patterns<br>
	 * listed in the annotations of this class.
	 *
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		System.out.println("A request has been received..............................");
		if (path.equals(gPROMSAgent.JOB_REQUEST_PATH)) {
			try {
				return setUpJob(requestParams.toString());
			} catch (SlurmJobException | IOException | gPROMSAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
			// } else if (path.equals(gPROMSAgent.JOB_OUTPUT_REQUEST_PATH)) {
			// JSONObject result = getSimulationResults(requestParams);
			// return result;
		} else if (path.equals(gPROMSAgent.JOB_STATISTICS_PATH)) {
			try {
				return produceStatistics(requestParams.toString());
			} catch (IOException | gPROMSAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else {
			System.out.println("Unknown request");
			throw new JPSRuntimeException(UNKNOWN_REQUEST);
		}
	}

	/**
	 * Validates input parameters specific to Kinetics Agent to decide whether<br>
	 * the job set up request can be served.
	 */
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
			throw new BadRequestException();
		}
		return true;
	}

	/**
	 * Checks the status of a job and returns results if it is finished and<br>
	 * post-processing is successfully completed. If the job has terminated<br>
	 * with an error or failed, then error termination message is sent.
	 *
	 * The JSON input for this request has the following format: {"jobId":
	 * "login-skylake.hpc.cam.ac.uk_117804308649998"}
	 *
	 * @param requestParams
	 * @return
	 */
	private JSONObject getSimulationResults(JSONObject requestParams) {
		JSONObject json = new JSONObject();
		String jobId = getJobId(requestParams);
		if (jobId == null) {
			return json.put("message", "jobId is not present in the request parameters.");
		}
		initAgentProperty();
		JSONObject message = checkJobInWorkspace(jobId);
		if (message != null) {
			return message;
		}
		JSONObject result = checkJobInCompletedJobs(jobId);
		if (result != null) {
			return result;
		}
		message = checkJobInFailedJobs(jobId);
		if (message != null) {
			return message;
		}
		return json.put("message", "The job is not available in the system.");
	}

	/**
	 * Checks the presence of the requested job in the workspace.<br>
	 * If the job is available, it returns that the job is currently running.
	 *
	 * @param json
	 * @return
	 */
	private JSONObject checkJobInWorkspace(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the set-up and running jobs folder.
		workspace = jobSubmission.getWorkspaceDirectory();
		if (workspace.isDirectory()) {
			File[] jobFolders = workspace.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					return json.put("message", "The job is being executed.");
				}
			}
		}
		return null;
	}

	/**
	 * Checks the presence of the requested job in the completed jobs.<br>
	 * If the job is available, it returns the result.
	 *
	 * @param json
	 * @return
	 */
	private JSONObject checkJobInCompletedJobs(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the completed jobs folder.
		String completedJobsPath = workspace.getParent().concat(File.separator)
				.concat(gPROMSAgentProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName());
		File completedJobsFolder = new File(completedJobsPath);
		if (completedJobsFolder.isDirectory()) {
			File[] jobFolders = completedJobsFolder.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					try {
						String inputJsonPath = completedJobsPath.concat(File.separator).concat(jobFolder.getName())
								.concat(File.separator).concat(gPROMSAgentProperty.getReferenceOutputJsonFile());
						InputStream inputStream = new FileInputStream(inputJsonPath);
						return new JSONObject(FileUtil.inputStreamToString(inputStream));
					} catch (FileNotFoundException e) {
						return json.put("message",
								"The job has been completed, but the file that contains results is not found.");
					}
				}
			}
		}
		return null;
	}

	/**
	 * Checks the presence of the requested job in the failed jobs.<br>
	 * If the job is available, it returns a message saying that<br>
	 * job has failed.
	 *
	 * @param json
	 * @param jobId
	 * @return
	 */
	private JSONObject checkJobInFailedJobs(String jobId) {
		JSONObject json = new JSONObject();
		// The path to the failed jobs folder.
		String failedJobsPath = workspace.getParent().concat(File.separator)
				.concat(gPROMSAgentProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName());
		File failedJobsFolder = new File(failedJobsPath);
		if (failedJobsFolder.isDirectory()) {
			File[] jobFolders = failedJobsFolder.listFiles();
			for (File jobFolder : jobFolders) {
				if (jobFolder.getName().equals(jobId)) {
					return json.put("message",
							"The job terminated with an error. Please check the failed jobs folder.");
				}
			}
		}
		return null;
	}

	/**
	 * Monitors already set up jobs.
	 *
	 * @throws SlurmJobException
	 */
	private void monitorJobs() throws SlurmJobException {
		// Configures all properties required for setting-up and running a Slurm job.
		jobSubmission.monitorJobs();

	}

	/**
	 * Sets up a quantum job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br. - the Status file.</br>
	 * - the JSON input file, which comes from the user request.</br>
	 *
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws gPROMSAgentException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, gPROMSAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("jobId", message);
		return obj;
	}

	/**
	 * Sets up the quantum job for the current input.
	 *
	 * @param jsonInput
	 * @return
	 * @throws IOException
	 * @throws gPROMSAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonInput)
			throws IOException, gPROMSAgentException, SlurmJobException {
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(gPROMSAgentProperty.getHpcAddress(), timeStamp);


		return jobSubmission.setUpJob(jsonInput,
				new File(URLDecoder.decode(getClass().getClassLoader().getResource(gPROMSAgentProperty.getSlurmScriptFileName())
						.getPath(), "utf-8")),
				new File("C:/Users/caresadmin/JParkSimulator-git/JPS_DIGITAL_TWIN/src/main/resources/input.zip"),
				timeStamp);
	}

	/**
	 * Produces a job folder name by following the schema hpcAddress_timestamp.
	 *
	 * @param hpcAddress
	 * @param timeStamp
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress, long timeStamp) {
		return hpcAddress.concat("_").concat("" + timeStamp);
	}

	/**
	 * Returns the job id.
	 *
	 * @param jsonObject
	 * @return
	 */
	public String getJobId(JSONObject jsonObject) {
		if (jsonObject.has("jobId")) {
			return jsonObject.get("jobId").toString();
		} else {
			return null;
		}

	}
}
