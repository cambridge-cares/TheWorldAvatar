package uk.ac.cam.cares.jps.agent.mechanism.sensana;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.commons.lang.StringUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana.MoDSFileMagtSensAna;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana.SensAnaResultsProcess;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.SftpException;

@Controller
@WebServlet(urlPatterns = {Property.JOB_REQUEST_PATH, Property.JOB_STATISTICS_PATH})
public class MoDSSensAnaAgent extends JPSAgent {
	private static final long serialVersionUID = 2L; //TODO to modify this
	private Logger logger = LoggerFactory.getLogger(MoDSSensAnaAgent.class);
	private File workspace;
	private String jobFolderPath;
	
	public static JobSubmission jobSubmission;
	public static ApplicationContext applicationContextMoDSSensAnaAgent;
	public static MoDSSensAnaAgentProperty modsSensAnaAgentProperty;
	
	public static final String REQUEST_RECEIVED = "A request to MoDSSensAnaAgent has been received..............................";
	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to MoDSSensAna Agent";
	
	/**
	 * Receives requests that match with the URL patterns listed in the<br>
	 * annotations of this class. 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		System.out.println(REQUEST_RECEIVED);
		if (path.equals(Property.JOB_REQUEST_PATH)) {
			try {
				validateInput(requestParams);
			} catch (BadRequestException e) {
				return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
			}
			try {
				return setUpJob(requestParams.toString());
			} catch (IOException | MoDSSensAnaAgentException | SlurmJobException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else if (path.equals(Property.JOB_STATISTICS_PATH)) {
			try {
				return produceStatistics(requestParams.toString());
			} catch (IOException | MoDSSensAnaAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else {
			System.out.println(UNKNOWN_REQUEST);
			throw new JPSRuntimeException(UNKNOWN_REQUEST);
		}
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) throws BadRequestException {
		if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
		try {
			String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(requestParams.toString());
			if (mechanismIRI == null || mechanismIRI.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_MECHANISM_IRI_MISSING.getPropertyName());
			}
			
			List<String> ignExpIRI = JSonRequestParser.getOntoChemExpIgnitionDelayIRI(requestParams.toString());
			List<String> flsExpIRI = JSonRequestParser.getOntoChemExpFlameSpeedIRI(requestParams.toString());
			if ((ignExpIRI == null || ignExpIRI.isEmpty()) 
					&& (flsExpIRI == null || flsExpIRI.isEmpty())) {
				throw new BadRequestException(Property.JOB_SETUP_EXPERIMENT_IRI_MISSING.getPropertyName());
			}
			
			String relPertur = JSonRequestParser.getRelPerturb(requestParams.toString());
			if (relPertur == null || relPertur.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_RELATIVE_PERTURBATION_MISSING.getPropertyName());
			}
			
			String maxAvg = JSonRequestParser.getMaxOrAvg(requestParams.toString());
			if (maxAvg != null && !maxAvg.isEmpty()) {
				if (!maxAvg.toLowerCase().contains("max") && !maxAvg.toLowerCase().contains("avg")) {
					throw new BadRequestException(Property.JOB_SETUP_MAX_AVG_INAPPROPRIATE.getPropertyName());
				}
			}
			
			String modsExePath = JSonRequestParser.getMoDSExePath(requestParams.toString());
			if (modsExePath == null || modsExePath.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_MODS_EXE_PATH_MISSING.getPropertyName());
			}
			
			String canteraEnv = JSonRequestParser.getCanteraCondaEnv(requestParams.toString());
			if (canteraEnv == null || canteraEnv.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_CANTERA_CONDA_ENV_MISSING.getPropertyName());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return true;
	}
	
	/**
	 * Shows the following statistics of sensitivity analysis jobs processed by MoDS Agent.
	 * - Total number of jobs submitted
	 * - Total number of jobs currently running
	 * - Total number of jobs successfully completed
	 * - Total number of jobs terminated with an error
	 * - Total number of jobs not started yet
	 * 
	 * @param input the JSON string specifying the return data format, e.g., JSON.
	 * @return the statistics in JSON format if requested.
	 */
	public JSONObject produceStatistics(@RequestParam String input) throws IOException, MoDSSensAnaAgentException {
		System.out.println("Received a request to send MoDSSensAnaAgent statistics.\n");
		logger.info("\nReceived a request to send MoDSSensAnaAgent statistics.\n");
		// Initialises all properties required for this agent to set-up<br>
		// and run jobs. It will also initialise the unique instance of<br>
		// Job Submission class.
		initAgentProperty();
		return jobSubmission.getStatistics(input);
	}
	
	/**
	 * Shows the following statistics of sensitivity analysis jobs processed by MoDS Agent. <br>
	 * This method covers the show statics URL that is not included in the <br>
	 * list of URL patterns.
	 * 
	 * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @return the statistics in HTML format.
	 */
	
	@RequestMapping(value=Property.JOB_SHOW_STATISTICS_PATH,method = RequestMethod.GET)
	@ResponseBody
	public String showStatistics() throws IOException, MoDSSensAnaAgentException {
		System.out.println("Received a request to show MoDSSensAnaAgent statistics.\n");
		logger.info("\nReceived a request to show MoDSSensAnaAgent statistics.\n");
		initAgentProperty();
		return jobSubmission.getStatistics();
	}
	
	/**
	 * Starts the asynchronous scheduler to monitor sensitivity analysis jobs.
	 * 
	 * @throws MoDSSensAnaAgentException
	 */
	public void init() throws ServletException {
		logger.info("\n---------- Sensitivity Analysis Agent has started ----------");
		System.out.println("---------- Sensitivity Analysis Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		MoDSSensAnaAgent modsSensAnaAgent = new MoDSSensAnaAgent();
		// initialising classes to read properties from the modssensana-agent.properties file
		initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				modsSensAnaAgent.monitorJobs();
			} catch (SlurmJobException e) 
			{e.printStackTrace();
			}
		}, 
				modsSensAnaAgentProperty.getAgentInitialDelayToStartJobMonitoring(), 
				modsSensAnaAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("\n---------- SensAna jobs are being monitored  ----------");
		System.out.println("---------- SensAna jobs are being monitored  ----------");
	}
	
	/**
	 * Initialises the unique instance of the MoDSSensAnaAgentProperty class that<br>
	 * reads all properties of MoDSSensAnaAgent from the modssensana-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modssensana-agent property file<br>
	 * through the MoDSSensAnaAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modssensana-agent.properties file
		if (applicationContextMoDSSensAnaAgent == null) {
			applicationContextMoDSSensAnaAgent = new AnnotationConfigApplicationContext(MoDSSensAnaAgentConfiguration.class);
		}
		if (modsSensAnaAgentProperty == null) {
			modsSensAnaAgentProperty = applicationContextMoDSSensAnaAgent.getBean(MoDSSensAnaAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsSensAnaAgentProperty.getAgentClass(), modsSensAnaAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(modsSensAnaAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(modsSensAnaAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(modsSensAnaAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(modsSensAnaAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(modsSensAnaAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(modsSensAnaAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(modsSensAnaAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(modsSensAnaAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(modsSensAnaAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(modsSensAnaAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(modsSensAnaAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(modsSensAnaAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(modsSensAnaAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(modsSensAnaAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(modsSensAnaAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(modsSensAnaAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Monitors already set up jobs. 
	 * 
	 * @throws SlurmJobException
	 */
	private void monitorJobs() throws SlurmJobException {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsSensAnaAgentProperty.getAgentClass(), modsSensAnaAgentProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	/**
	 * Monitors the currently running calibration jobs to allow new jobs to start.
	 * In doing so , it checks if the number of running jobs is less than the 
	 * maximum number of jobs allowed to run at a time. 
	 * 
	 */
	private void processOutputs() {
		initAgentProperty();
		workspace = jobSubmission.getWorkspaceDirectory();
		try {
			if (workspace.isDirectory()) {
				File[] jobFolders = workspace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							selectSensRxns(jobFolder);
							updateJobOutputStatus(jobFolder);
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (SftpException e) {
			e.printStackTrace();
		} catch (JSchException e) {
			e.printStackTrace();
		} catch (MoDSSensAnaAgentException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Updates the output status of a completed job.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws JSchException
	 * @throws SftpException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean updateJobOutputStatus(File jobFolder) throws JSchException, SftpException, IOException, InterruptedException {
		File statusFile = Utils.getStatusFile(jobFolder);
		return updateJobOutputStatus(jobFolder.getName(), statusFile);
	}
	
	/**
	 * Select the reactions to be optimised based on sensitivity analysis results and reactions must included that provided by user. 
	 * 
	 * @param jobFolder
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 */
	public void selectSensRxns(File jobFolder) throws IOException, MoDSSensAnaAgentException {
		File outputFile = new File(jobFolder.getAbsolutePath());
		String zipFilePath = jobFolder.getAbsolutePath().concat(File.separator).concat(modsSensAnaAgentProperty.getOutputFileName()).concat(modsSensAnaAgentProperty.getOutputFileExtension());
		String destDir = jobFolder.getAbsolutePath().concat(File.separator).concat(modsSensAnaAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(jobFolder.getAbsolutePath()
				.concat(File.separator)
				.concat(modsSensAnaAgentProperty.getJsonInputFileName())
				.concat(modsSensAnaAgentProperty.getJsonFileExtension())));
		JsonNode inputNode = new ObjectMapper().readTree(jsonString);
		
		SensAnaResultsProcess sensAnaRePro = new SensAnaResultsProcess(modsSensAnaAgentProperty);
		List<String> selectedRxns = sensAnaRePro.processResults(destDir, jsonString);
		
		if (selectedRxns != null && !selectedRxns.isEmpty()) {
			updateJsonForCalib(inputNode, selectedRxns, destDir.concat(File.separator).concat("modifiedInput.json"));
		}
		
		System.out.println("Sensitivity analysis results were successfully processed.");
	}
	
	/**
	 * Read content from json input file. 
	 * 
	 * @param input
	 * @return
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 */
	protected String readJsonInput(File input) throws IOException, MoDSSensAnaAgentException {
		String jsonString = new String();
		BufferedReader br = null;
		br = new BufferedReader(new InputStreamReader(new FileInputStream(input)));
		String line = new String();
		while ((line = br.readLine()) != null) {
			jsonString = jsonString.concat(line);
		}
		br.close();
		return jsonString;
	}
	
	/**
	 * Add list of IRI of reactions to be optimised to json input so that forms input json for MoDSMechCalibAgent. 
	 * @param inputNode
	 * @param selectedRxns
	 * @param output
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 */
	protected void updateJsonForCalib(JsonNode inputNode, List<String> selectedRxns, String output) throws IOException, MoDSSensAnaAgentException {
		JsonNode locateNode = inputNode.path("json").path("ontokinIRI");
		String rxns = new String();
		for (String rxn : selectedRxns) {
			rxns = rxns.concat(", \"").concat(rxn).concat("\"");
		}
		ObjectNode addedNode = ((ObjectNode) locateNode).set("reactionList", new ObjectMapper().readTree("["+rxns.substring(1)+"]"));

		BufferedWriter bw = null;
		bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(output))));
		bw.write(inputNode.toString());
		bw.close();
	}
	
	/**
	 * Updates the latest status of the running jobs. 
	 * 
	 * @param runningJob
	 * @param statusFile
	 * @return
	 * @throws JSchException
	 * @throws SftpException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean updateJobOutputStatus(String completedJob, File statusFile) throws JSchException, SftpException, IOException, InterruptedException {
		if (statusFile!=null) {
			Utils.modifyOutputStatus(statusFile.getAbsolutePath(), Status.OUTPUT_PROCESSED.getName());
			return true;
		}
		return false;
	}
	
	/**
	 * Sets up a mechanism calibration job by creating the job folder and the following files
	 * under this folder:
	 * - the JSON input file, which comes from the user request.
	 * - the InputParams.xml file.
	 * - the chemical_mechanism file.
	 * - the associated CSV files for MoDS.
	 * - the MoDS_inpus.xml file.
	 * - the Slurm script file.
	 * - the status file.
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 * @throws SlurmJobException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("jobFolderPath", message);
		return obj;
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(modsSensAnaAgentProperty.getHpcAddress(), timeStamp);
		String setUpMsg = jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(modsSensAnaAgentProperty.getSlurmScriptFileName()).getPath()), 
				getInputFile(jsonString, jobFolderName), timeStamp);
		if (setUpMsg != null) {
			deleteDirectory(new File(jobFolderPath));
			return jobSubmission.getWorkspaceDirectory().getAbsolutePath().concat(File.separator).concat(jobFolderName);
		}
		return null;
	}
	
	/**
	 * Sets up the calibration job for the current request.
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSSensAnaAgentException
	 */
	private File getInputFile(String jsonString, String jobFolderName) throws IOException, MoDSSensAnaAgentException {
		MoDSFileMagtSensAna fileMagt = new MoDSFileMagtSensAna(modsSensAnaAgentProperty);
		
		jobFolderPath = fileMagt.createMoDSJob(jsonString, jobFolderName);
		
		return Utils.getZipFile(new File(jobFolderPath).getAbsolutePath());
	}
	
	/**
	 * Produces a job folder name by following the schema hpcAddress_timestamp.
	 * 
	 * @param hpcAddress
	 * @param timeStamp
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress, long timeStamp){
		return hpcAddress.concat("_").concat("" + timeStamp);
	}
	
	/**
	 * Retrieves the timestamp part from the name of a job folder.<br>
	 * A job folder consists of hpcAddress_timestamp, for example,<br>
	 * from the job folder name login-skylake.hpc.cam.ac.uk_1086309217579500,<br>
	 * this method returns 1086309217579500. This timestamp is appended to<br>
	 * the name of the slurm input file. The corresponding Slurm script file<br>
	 * name can be login-skylake.hpc.cam.ac.uk_1086309217579500.com.  
	 * 
	 * @param folder
	 * @return
	 */
	public String getTimeStampPart(String folder) {
		if (folder.contains("_")) {
			String[] tokens = folder.split("_");
			if (tokens.length==2 && tokens[1]!=null && StringUtils.isNumeric(tokens[1])) {
				return tokens[1];
			}
		}
		return null;
	}
	
	/**
	 * Delete the temporary directory that generated during creating MoDS job. 
	 * 
	 * @param directoryToBeDeleted
	 */
	protected void deleteDirectory(File directoryToBeDeleted) throws IOException {
	    File[] allContents = directoryToBeDeleted.listFiles();
	    if (allContents != null) {
	        for (File file : allContents) {
	            deleteDirectory(file);
	        }
	    }
	    directoryToBeDeleted.delete();
	}
}
