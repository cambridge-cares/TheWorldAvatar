package uk.ac.cam.cares.jps.agent.mechanism.calibration;

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

import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSMechCalibAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MechCalibOutputProcess;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

@Controller
@WebServlet(urlPatterns = {Property.JOB_REQUEST_PATH, Property.JOB_STATISTICS_PATH})
public class MoDSMechCalibAgent extends JPSAgent {
	private static final long serialVersionUID = 2L; //TODO to modify this
	private Logger logger = LoggerFactory.getLogger(MoDSMechCalibAgent.class);
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
	public static MoDSMechCalibAgentProperty modsMechCalibAgentProperty;
	
	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to MoDSMechCalib Agent";
	
	public static void main(String[] args) throws ServletException, MoDSMechCalibAgentException {
		MoDSMechCalibAgent modsMechCalibAgent = new MoDSMechCalibAgent();
//		modsMechCalibAgent.init();
		String input = "{\"json\":{\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261\",\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264155_173\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264148_166\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264020_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264017_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264156_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264154_172\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264157_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264152_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264053_71\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264158_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264104_122\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264135_153\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264142_160\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264134_152\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264165_183\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264137_155\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264159_177\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264136_154\"]},\"mods\":{\"ignDelayOption\":{\"method\":\"1\",\"species\":\"AR\"},\"flameSpeedOption\":{\"tranModel\":\"mix-average\"},\"sensAna\":{\"topN\":\"10\",\"relPerturbation\":\"1e-3\"}}}}";
	}
	
	/**
	 * Receives requests that match with the URL patterns listed in the<br>
	 * annotations of this class. 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		MoDSMechCalibAgent modsMechCalibAgent = new MoDSMechCalibAgent();
		System.out.println("A request has been received..............................");
		if (path.equals(Property.JOB_REQUEST_PATH)) {
			try {
				validateInput(requestParams);
			} catch (BadRequestException e) {
				return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
			}
			try {
				return modsMechCalibAgent.setUpJob(requestParams.toString());
			} catch (IOException | MoDSMechCalibAgentException | SlurmJobException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else if (path.equals(Property.JOB_STATISTICS_PATH)) {
			try {
				return modsMechCalibAgent.produceStatistics(requestParams.toString());
			} catch (IOException | MoDSMechCalibAgentException e) {
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
			if (rxnIRI == null || rxnIRI.isEmpty()) {
				throw new BadRequestException(Property.JOB_SETUP_REACTION_IRI_MISSING.getPropertyName());
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
	public JSONObject produceStatistics(String input) throws IOException, MoDSMechCalibAgentException {
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
	public String showStatistics() throws IOException, MoDSMechCalibAgentException {
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
	 * @throws MoDSMechCalibAgentException
	 */
	public void init() throws ServletException {
		logger.info("---------- Mechanism Calibration Agent has started ----------");
		System.out.println("---------- Mechanism Calibration Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		MoDSMechCalibAgent modsMechCalibAgent = new MoDSMechCalibAgent();
		// initialising classes to read properties from the mods-agent.properties file
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (applicationContextMoDSAgent == null) {
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(MoDSMechCalibAgentConfiguration.class);
		}
		if (modsMechCalibAgentProperty == null) {
			modsMechCalibAgentProperty = applicationContextMoDSAgent.getBean(MoDSMechCalibAgentProperty.class);
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
		logger.info("---------- Calibration jobs are being monitored  ----------");
		System.out.println("---------- Calibration jobs are being monitored  ----------");
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
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(MoDSMechCalibAgentConfiguration.class);
		}
		if (modsMechCalibAgentProperty == null) {
			modsMechCalibAgentProperty = applicationContextMoDSAgent.getBean(MoDSMechCalibAgentProperty.class);
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
		} catch (MoDSMechCalibAgentException e) {
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
	
	public void updateCalibMech(File jobFolder) throws IOException, MoDSMechCalibAgentException {
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
	
	private String readJsonInput(File input) throws IOException, MoDSMechCalibAgentException {
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
	 * @throws MoDSMechCalibAgentException
	 * @throws SlurmJobException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, MoDSMechCalibAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("message", message);
		return obj;
	}
	
	/**
	 * Sets up the calibration job for the current input. 
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 * @throws SlurmJobException
	 */
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, MoDSMechCalibAgentException, SlurmJobException {
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
		String jobFolderName = getNewJobFolderName(slurmJobProperty.getHpcAddress(), timeStamp);
		return jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()), 
				getInputFile(jsonString, jobFolderName), timeStamp);
	}
	
	/**
	 * Sets up the calibration job for the current request.
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	private File getInputFile(String jsonString, String jobFolderName) throws IOException, MoDSMechCalibAgentException {
		MoDSFileManagement fileMagt = new MoDSFileManagement();
		
		String jobFolderPath = fileMagt.createMoDSJob(jsonString, jobFolderName);
		
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
	
	
}
