package uk.ac.cam.cares.jps.agent.mechanism.coordination;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
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
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.AutoMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MechCalibOutputProcess;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.coordination.AutoMechCalibAgentException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Workspace;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

@Controller
@WebServlet(urlPatterns = {Property.JOB_COORDINATION_PATH})
public class AutoMechCalibAgent extends JPSAgent {
	private static final long serialVersionUID = 2L; //TODO to modify this
	private Logger logger = LoggerFactory.getLogger(AutoMechCalibAgent.class);
	String server = "login-cpu.hpc.cam.ac.uk";
	String username = "jb2197";
	String password = new String();
	boolean isAuthenticated;
	private File jobSpace;
	
	static Session session;
	static JSch jsch = new JSch();
	
	static int scheduledIteration = 0;
	static List<String> jobsRunning = new ArrayList<>();
	
	SlurmJob slurmJob = new SlurmJob();
	public static JobSubmission jobSubmission;
	public static ApplicationContext applicationContext;
	public static SlurmJobProperty slurmJobProperty;
	public static ApplicationContext applicationContextMoDSAgent;
	public static AutoMechCalibAgentProperty autoMechCalibAgentProperty;
	
	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to AutoMechCalib Agent";
	
	public static void main(String[] args) throws ServletException, AutoMechCalibAgentException {
		
	}
	
	/**
	 * Receives requests that match with the URL patterns listed in the<br>
	 * annotations of this class. 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		AutoMechCalibAgent autoMechCalibAgent = new AutoMechCalibAgent();
		System.out.println("A request has been received..............................");
		
		if (path.equals(Property.JOB_COORDINATION_PATH)) {
			try {
				validateInput(requestParams);
			} catch (BadRequestException e) {
				return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
			}
			try {
				return autoMechCalibAgent.setUpJob(requestParams.toString());
			} catch (IOException | AutoMechCalibAgentException | SlurmJobException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else if (path.equals(Property.JOB_STATISTICS_PATH)) {
			try {
				return autoMechCalibAgent.produceStatistics(requestParams.toString());
			} catch (IOException | AutoMechCalibAgentException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else {
			System.out.println("Unknown request");
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
			
			List<String> rxnIRI = JSonRequestParser.getOntoKinReactionsIRI(requestParams.toString());
			if (rxnIRI != null && !rxnIRI.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_REACTION_IRI_MISPLACED.getPropertyName());
			}
			
			String relPertur = JSonRequestParser.getRelPerturb(requestParams.toString());
			if (relPertur == null || relPertur.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_RELATIVE_PERTURBATION_MISSING.getPropertyName());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return true;
	}
	
	/**
	 * Shows the following statistics of calibration jobs processed by MoDS Agent.
	 * - Total number of jobs submitted
	 * - Total number of jobs currently running
	 * - Total number of jobs successfully completed
	 * - Total number of jobs terminated with an error
	 * - Total number of jobs not started yet
	 * 
	 * @param input the JSON string specifying the return data format, e.g., JSON.
	 * @return the statistics in JSON format if requested.
	 */
	public JSONObject produceStatistics(String input) throws IOException, AutoMechCalibAgentException {
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		if (jobSubmission==null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics(input);
	}
	
	/**
	 * Shows the following statistics of calibration jobs processed by MoDS Agent. <br>
	 * This method covers the show statics URL that is not included in the<br>
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
	public String showStatistics() throws IOException, AutoMechCalibAgentException {
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		if (jobSubmission==null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics();
	}
	
	/**
	 * Starts the asynchronous scheduler to monitor calibration jobs.
	 * 
	 * @throws AutoMechCalibAgentException
	 */
	public void init() throws ServletException {
		logger.info("---------- Automated Mechanism Calibration Agent has started ----------");
		System.out.println("---------- Automated Mechanism Calibration Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		AutoMechCalibAgent modsMechCalibAgent = new AutoMechCalibAgent();
		// initialising classes to read properties from the mods-agent.properties file
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (applicationContextMoDSAgent == null) {
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(AutoMechCalibAgentConfiguration.class);
		}
		if (autoMechCalibAgentProperty == null) {
			autoMechCalibAgentProperty = applicationContextMoDSAgent.getBean(AutoMechCalibAgentProperty.class);
		}
		// the first 30 refers to the delay (in seconds) before the job scheduler
		// starts and the second 60 refers to the interval between two consecutive
		// executions of the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				modsMechCalibAgent.monitorJobs();
			} catch (SlurmJobException e) {
				e.printStackTrace();
			}
		}, 
				slurmJobProperty.getAgentInitialDelayToStartJobMonitoring(), 
				slurmJobProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("---------- Automated Calibration jobs are being monitored  ----------");
		System.out.println("---------- Automated Calibration jobs are being monitored  ----------");
	}
	
	private void monitorJobs() throws SlurmJobException {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		try {
			jobSubmission.monitorJobs();
		} catch (SlurmJobException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		processOutputs();
	}
	
	/**
	 * Monitors the currently running calibration jobs to allow new jobs to start.
	 * In doing so , it checks if the number of running jobs is less than the 
	 * maximum number of jobs allowed to run at a time. 
	 * 
	 */
	private void processOutputs() {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (jobSubmission==null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		if (applicationContextMoDSAgent == null) {
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(AutoMechCalibAgentConfiguration.class);
		}
		if (autoMechCalibAgentProperty == null) {
			autoMechCalibAgentProperty = applicationContextMoDSAgent.getBean(AutoMechCalibAgentProperty.class);
		}
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							updateCalibMech(jobFolder);
							updateJpbOutputStatus(jobFolder);
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
		} catch (AutoMechCalibAgentException e) {
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
	private boolean updateJpbOutputStatus(File jobFolder) throws JSchException, SftpException, IOException, InterruptedException {
		File statusFile = Utils.getStatusFile(jobFolder);
		return updateJobOutputStatus(jobFolder.getName(), statusFile);
	}
	
	public void updateCalibMech(File jobFolder) throws IOException, AutoMechCalibAgentException {
		File outputFile = new File(jobFolder.getAbsolutePath());
		String zipFilePath = jobFolder.getAbsolutePath().concat(File.separator).concat(slurmJobProperty.getOutputFileName()).concat(slurmJobProperty.getOutputFileExtension());
		String destDir = jobFolder.getAbsolutePath().concat(File.separator).concat(slurmJobProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(jobFolder.getAbsolutePath()
				.concat(File.separator)
				.concat(slurmJobProperty.getJsonInputFileName())
				.concat(slurmJobProperty.getJsonFileExtension())));
		
		String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(jsonString);
		List<String> reactionIRIList = JSonRequestParser.getOntoKinReactionsIRI(jsonString);
		
		MechCalibOutputProcess mechCalibPro = new MechCalibOutputProcess();
		mechCalibPro.processResults(destDir, mechanismIRI, reactionIRIList, String.valueOf(Utils.getTimeStamp()));
		
		System.out.println("Mechanism calibration results were successfully processed.");
	}
	
	private String readJsonInput(File input) throws IOException, AutoMechCalibAgentException {
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
	 * Set up an automated mechanism calibration job. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("message", message);
		return obj;
	}
	
	/**
	 * Set up the automated mechanism calibration job on agent machine. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), 
					slurmJobProperty.getHpcAddress());
		}
		long timeStamp = Utils.getTimeStamp();
		if (jobSubmission.getWorkspaceDirectory()==null) {
			return Status.JOB_SETUP_ERROR.getName();
		} else {
			Workspace ws = new Workspace();
			File workspaceFolder = jobSubmission.getWorkspaceDirectory();
			File jobFolder = ws.createJobFolder(workspaceFolder.getAbsolutePath(), slurmJobProperty.getHpcAddress(), timeStamp);
			String statusFileMsg = ws.createStatusFile(workspaceFolder, ws.getStatusFilePath(jobFolder), slurmJobProperty.getHpcAddress());
			if (statusFileMsg == null) {
				return null;
			}
			String jsonInputFileMsg = ws.createJSONInputFile(workspaceFolder, ws.getJSONInputFilePath(jobFolder), jsonString);
			if (jsonInputFileMsg == null) {
				return null;
			}
			return setUpSensAnaJob(jobFolder.getAbsolutePath(), jsonString);
		}
	}
	
	/**
	 * Set up a mechanism sensitivity analysis job by sending<br>
	 * HTTP request to execute MoDSSensAnaAgent. 
	 * 
	 * @param autoJobFolderPath
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	public String setUpSensAnaJob(String autoJobFolderPath, String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		return setUpAgentJob(autoJobFolderPath, 
				Property.AGENT_SENS_ANA_PATH.concat(Property.JOB_REQUEST_PATH), jsonString);
	}
	
	/**
	 * Set up a mechanism calibration job by sending HTTP<br>
	 * request to execute MoDSMechCalibAgent. 
	 * 
	 * @param autoJobFolderPath
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	public String setUpMechCalibJob(String autoJobFolderPath, String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		return setUpAgentJob(autoJobFolderPath, 
				Property.AGENT_MECH_CALIB_PATH.concat(Property.JOB_REQUEST_PATH), jsonString);
	}
	
	/**
	 * Set up an agent job by sending HTTP request to execute<br>
	 * the specified agent, then copy the generated job file to<br>
	 * the automated mechanism calibration job folder. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws AutoMechCalibAgentException
	 * @throws SlurmJobException
	 */
	public String setUpAgentJob(String localFolderPath, String agentPath, String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
		String srcJobPath = execute(agentPath, jsonString);
		if (srcJobPath != null && !srcJobPath.isEmpty()) {
			if (srcJobPath.endsWith("\\")) {
				srcJobPath = srcJobPath.substring(0, srcJobPath.lastIndexOf("\\"));
			}
			if (localFolderPath.endsWith("\\")) {
				localFolderPath = localFolderPath.substring(0, localFolderPath.lastIndexOf("\\"));
			}
			String destJobPath = localFolderPath.concat(File.separator).concat(srcJobPath.substring(srcJobPath.lastIndexOf("\\")));
			Utils.copyFolder(srcJobPath, destJobPath);
			
			return Status.JOB_SETUP_SUCCESS_MSG.getName(); 
		}
		return null;
	}
	
//	/**
//	 * Sets up the calibration job for the current input. 
//	 * 
//	 * @param jsonString
//	 * @return
//	 * @throws IOException
//	 * @throws AutoMechCalibAgentException
//	 * @throws SlurmJobException
//	 */
//	private String setUpJobOnAge ntMachine(String jsonString) throws IOException, AutoMechCalibAgentException, SlurmJobException {
//		if (applicationContext == null) {
//			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
//		}
//		if (slurmJobProperty == null) {
//			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
//		}
//		if (jobSubmission == null) {
//			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), 
//					slurmJobProperty.getHpcAddress());
//		}
//		long timeStamp = Utils.getTimeStamp();
//		String jobFolderName = getNewJobFolderName(slurmJobProperty.getHpcAddress(), timeStamp);
//		return jobSubmission.setUpJob(jsonString, 
//				new File(getClass().getClassLoader().getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()), 
//				getInputFile(jsonString, jobFolderName), timeStamp);
//	}
	
//	/**
//	 * Sets up the calibration job for the current request.
//	 * 
//	 * @param jsonString
//	 * @return
//	 * @throws IOException
//	 * @throws AutoMechCalibAgentException
//	 */
//	private File getInputFile(String jsonString, String jobFolderName) throws IOException, AutoMechCalibAgentException {
//		MoDSFileManagement fileMagt = new MoDSFileManagement();
//		
//		String jobFolderPath = fileMagt.createMoDSJob(jsonString, jobFolderName);
//		
//		return Utils.getZipFile(new File(jobFolderPath).getAbsolutePath());
//	}
	
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
	
	
}
