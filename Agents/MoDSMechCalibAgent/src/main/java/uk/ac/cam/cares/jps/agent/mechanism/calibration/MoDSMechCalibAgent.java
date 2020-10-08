package uk.ac.cam.cares.jps.agent.mechanism.calibration;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.LinkedHashMap;
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
import uk.ac.cam.cares.jps.agent.file_management.marshallr.yrt23.MoDS4yrt23;
import uk.ac.cam.cares.jps.agent.json.parser.JSonRequestParser;
import uk.ac.cam.cares.jps.agent.mechanism.calibration.MoDSMechCalibAgentException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.kg.OntoKinKG;

import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.SftpException;

@Controller
@WebServlet(urlPatterns = {Property.JOB_REQUEST_PATH, Property.JOB_STATISTICS_PATH, Property.JOB_REQUEST_EVAL_PATH})
public class MoDSMechCalibAgent extends JPSAgent {
	private static final long serialVersionUID = 2L; //TODO to modify this
	private Logger logger = LoggerFactory.getLogger(MoDSMechCalibAgent.class);
	private File workspace;
	private String jobFolderPath;
	
	public static JobSubmission jobSubmission;
	public static ApplicationContext applicationContextMoDSMechCalibAgent;
	public static MoDSMechCalibAgentProperty modsMechCalibAgentProperty;
	
	public static final String REQUEST_RECEIVED = "A request to MoDSMechCalibAgent has been received..............................";
	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to MoDSMechCalib Agent";
	public static final String OUTPUT_PROCESS_FAILED = "Mechanism calibration results process were failed.";
	public static final String EVAL_CHECK_FAILED = "Mechanism evaluation results were not found.";
	
	public static void main(String[] args) {
		MoDSMechCalibAgent test = new MoDSMechCalibAgent();
		String[] ratio = new String[] {"6.3","4.98","3.65","2.33","1.0"};
		for (String r : ratio) {
			String jsonString = "{\"json\":{\"mods\":{"
					+ "\"calibrationAlg\":{\"epsilon\":\"0.001\",\"initPoints\":\"3\",\"objectiveFunction\":\"SumOfSquares\","
					+ "\"rho\":\"0.2\",\"rhoFactor\":\"0.5\",\"nIters\":\"400\"},"
					+ "\"sensAna\":{\"relPerturbation\":\"1e-3\",\"maxORavg\":\"max\",\"topN\":\"10\"},"
					+ "\"samplingAlg\":{\"outputInterval\":\"1000\",\"rangeOfMultipliers\":\"100.0\",\"sobolPoints\":\"10000\",\"activeParamScaling\":\"logarithmic\"},"
					+ "\"ignDelayOption\":{\"method\":\"1\",\"species\":\"AR\",\"scaling\":\"logarithmic\"},"
					+ "\"flameSpeedOption\":{\"tranModel\":\"mix-average\",\"scaling\":\"linear\",\"responseRatio\":"
					+ "\"" + r + "\"},"
					+ "\"executable\":{\"path\":\"/home/jb2197/Codes_kinetics/mods-backend/outputs/Release/bin/MoDS_mpi\"}},"
					+ "\"cantera\":{\"environment\":\"pycantera\"}," + "\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261\",\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264148_166\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264157_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264156_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264020_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264155_173\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264017_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264158_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264053_71\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264152_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264163_181\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264104_122\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264135_153\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264165_183\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264134_152\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264142_160\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264154_172\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264136_154\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264137_155\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ChemicalReaction_73656018264159_177\"]},"
					+ "\"kinetics\":{\"numerical\":{\"simEnd\":\"500\"}}}}";
			test.initAgentProperty();
			try {
				test.setUpJob(jsonString);
			} catch (IOException | MoDSMechCalibAgentException | SlurmJobException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Receives requests that match with the URL patterns listed in the<br>
	 * annotations of this class. 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		String path = request.getServletPath();
		System.out.println(REQUEST_RECEIVED);
		if (path.equals(Property.JOB_REQUEST_PATH) || path.equals(Property.JOB_REQUEST_EVAL_PATH)) {
			try {
				validateInput(requestParams);
			} catch (BadRequestException e) {
				return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
			}
			try {
				requestParams = validateRxnMustInclude(requestParams);
				return setUpJob(requestParams.toString());
			} catch (IOException | MoDSMechCalibAgentException | SlurmJobException e) {
				throw new JPSRuntimeException(e.getMessage());
			}
		} else if (path.equals(Property.JOB_STATISTICS_PATH)) {
			try {
				return produceStatistics(requestParams.toString());
			} catch (IOException | MoDSMechCalibAgentException e) {
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
			
			boolean onlyEval = JSonRequestParser.getIfOnlyEval(requestParams.toString());
			if (!onlyEval) {
				List<String> rxnIRI = JSonRequestParser.getOntoKinReactionsIRI(requestParams.toString());
				if (rxnIRI == null || rxnIRI.isEmpty()) {
					List<String> rxnsMustInclude = JSonRequestParser.getRxnsMustInclude(requestParams.toString());
					if (rxnsMustInclude == null || rxnsMustInclude.isEmpty()) {
						throw new BadRequestException(Property.JOB_SETUP_REACTION_IRI_MISSING.getPropertyName());
					}
				} else {
					OntoKinKG ontoKinKg = new OntoKinKG(modsMechCalibAgentProperty);
					LinkedHashMap<String, String> results;
					try {
						results = ontoKinKg.queryReactionsToOptimise(mechanismIRI, rxnIRI);
						if (results == null || results.isEmpty()) {
							throw new BadRequestException(Property.JOB_SETUP_MECHANISM_REACTION_MISMATCH.getPropertyName());
						}
					} catch (MoDSMechCalibAgentException e) {
						e.printStackTrace();
					}
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
	 * Validate if the reactions must include are added to list of reactions to be optimised, and add those not included reactions. 
	 * 
	 * @param requestParams
	 * @return
	 * @throws MoDSMechCalibAgentException
	 */
	public JSONObject validateRxnMustInclude(JSONObject requestParams) throws MoDSMechCalibAgentException {
		try {
			boolean onlyEval = JSonRequestParser.getIfOnlyEval(requestParams.toString());
			if (!onlyEval) {
				List<String> rxnIRI = JSonRequestParser.getOntoKinReactionsIRI(requestParams.toString());
				List<String> rxnsMustInclude = JSonRequestParser.getRxnsMustInclude(requestParams.toString());
				String mechanismIRI = JSonRequestParser.getOntoKinMechanismIRI(requestParams.toString());
				if (rxnsMustInclude != null && !rxnsMustInclude.isEmpty()) {
					for (String rxn : rxnsMustInclude) {
						OntoKinKG ontoKinKg = new OntoKinKG(modsMechCalibAgentProperty);
						LinkedHashMap<String, String> rxnIRIandEqu = ontoKinKg.queryReactionBasedOnNo(mechanismIRI, rxn);
						for (String iri : rxnIRIandEqu.keySet()) {
							if (!rxnIRI.contains(iri)) {
								requestParams.getJSONObject("json").getJSONObject("ontokinIRI").getJSONArray("reactionList").put(iri);
							}
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return requestParams;
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
		System.out.println("Received a request to send MoDSMechCalibAgent statistics.\n");
		logger.info("\nReceived a request to send MoDSMechCalibAgent statistics.\n");
		// Initialises all properties required for this agent to set-up<br>
		// and run jobs. It will also initialise the unique instance of<br>
		// Job Submission class.
		initAgentProperty();
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
		System.out.println("Received a request to show MoDSMechCalibAgent statistics.\n");
		logger.info("\nReceived a request to show MoDSMechCalibAgent statistics.\n");
		initAgentProperty();
		return jobSubmission.getStatistics();
	}
	
	/**
	 * Starts the asynchronous scheduler to monitor calibration jobs.
	 * 
	 * @throws MoDSMechCalibAgentException
	 */
	public void init() throws ServletException {
		logger.info("\n---------- Mechanism Calibration Agent has started ----------");
		System.out.println("---------- Mechanism Calibration Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		MoDSMechCalibAgent modsMechCalibAgent = new MoDSMechCalibAgent();
		// initialising classes to read properties from the modsmechcalib-agent.properties file
		initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				modsMechCalibAgent.monitorJobs();
			} catch (SlurmJobException e) {
				e.printStackTrace();
			}
		}, 
				modsMechCalibAgentProperty.getAgentInitialDelayToStartJobMonitoring(), 
				modsMechCalibAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("\n---------- MechCalib jobs are being monitored  ----------");
		System.out.println("---------- MechCalib jobs are being monitored  ----------");
	}
	
	/**
	 * Initialises the unique instance of the MoDSMechCalibAgentProperty class that<br>
	 * reads all properties of MoDSMechCalibAgentProperty from the modsmechcalib-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the modsmechcalib-agent property file<br>
	 * through the MoDSMechCalibAgentProperty class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the modsmechcalib-agent.properties file
		if (applicationContextMoDSMechCalibAgent == null) {
			applicationContextMoDSMechCalibAgent = new AnnotationConfigApplicationContext(MoDSMechCalibAgentConfiguration.class);
		}
		if (modsMechCalibAgentProperty == null) {
			modsMechCalibAgentProperty = applicationContextMoDSMechCalibAgent.getBean(MoDSMechCalibAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsMechCalibAgentProperty.getAgentClass(), modsMechCalibAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(modsMechCalibAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserPassword(modsMechCalibAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(modsMechCalibAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty.setAgentCompletedJobsSpacePrefix(modsMechCalibAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setAgentFailedJobsSpacePrefix(modsMechCalibAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(modsMechCalibAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(modsMechCalibAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(modsMechCalibAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(modsMechCalibAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(modsMechCalibAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(modsMechCalibAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(modsMechCalibAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(modsMechCalibAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(modsMechCalibAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(modsMechCalibAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty.setAgentPeriodicActionInterval(modsMechCalibAgentProperty.getAgentPeriodicActionInterval());
		}
	}
	
	/**
	 * Monitors already set up jobs. 
	 * 
	 * @throws SlurmJobException
	 */
	private void monitorJobs() throws SlurmJobException {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(modsMechCalibAgentProperty.getAgentClass(), modsMechCalibAgentProperty.getHpcAddress());
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
							updateCalibMech(jobFolder);
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
	private boolean updateJobOutputStatus(File jobFolder) throws JSchException, SftpException, IOException, InterruptedException {
		File statusFile = Utils.getStatusFile(jobFolder);
		return updateJobOutputStatus(jobFolder.getName(), statusFile);
	}
	
	/**
	 * Update the mechanism with optimised rate parameters. 
	 * 
	 * @param jobFolder
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	public void updateCalibMech(File jobFolder) throws IOException, MoDSMechCalibAgentException {
		File outputFile = new File(jobFolder.getAbsolutePath());
		String zipFilePath = jobFolder.getAbsolutePath().concat(File.separator).concat(modsMechCalibAgentProperty.getOutputFileName()).concat(modsMechCalibAgentProperty.getOutputFileExtension());
		String destDir = jobFolder.getAbsolutePath().concat(File.separator).concat(modsMechCalibAgentProperty.getOutputFileName());
		Utils.unzipFile(zipFilePath, destDir);
		
		String jsonString = readJsonInput(new File(jobFolder.getAbsolutePath()
				.concat(File.separator)
				.concat(modsMechCalibAgentProperty.getJsonInputFileName())
				.concat(modsMechCalibAgentProperty.getJsonFileExtension())));
		
		boolean onlyEval = JSonRequestParser.getIfOnlyEval(jsonString);
		if (!onlyEval) {
			MechCalibOutputProcess mechCalibPro = new MechCalibOutputProcess(modsMechCalibAgentProperty);
			String mechOwlOnServer = mechCalibPro.processResults(destDir, jsonString);
			if (mechOwlOnServer != null && !mechOwlOnServer.isEmpty()) {
				System.out.println("Mechanism calibration results were successfully processed.");
			} else {
				throw new JPSRuntimeException(OUTPUT_PROCESS_FAILED);
			}
		} else {
			boolean evalResults = checkEvalResults(destDir);
			if (evalResults) {
				System.out.println("Mechanism evaluation were successfully finished.");
			} else {
				throw new JPSRuntimeException(EVAL_CHECK_FAILED);
			}
		}
	}
	
	/**
	 * Read content from json file. 
	 * 
	 * @param input
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	protected String readJsonInput(File input) throws IOException, MoDSMechCalibAgentException {
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
	 * Check if evaluation folder exist. 
	 * 
	 * @param jobFolderPath
	 * @return
	 * @throws IOException
	 * @throws MoDSMechCalibAgentException
	 */
	private boolean checkEvalResults(String jobFolderPath) throws IOException, MoDSMechCalibAgentException {
		if (jobFolderPath.endsWith("\\")) {
			jobFolderPath = jobFolderPath.substring(0,jobFolderPath.length()-1);
		}
		File evalFolder = new File(jobFolderPath.concat(File.separator).concat("Evaluation"));
		if (evalFolder.exists()) {
			return true;
		}
		
		return false;
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
		obj.put("jobFolderPath", message);
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
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(modsMechCalibAgentProperty.getHpcAddress(), timeStamp);
		String setUpMsg = jobSubmission.setUpJob(jsonString, 
				new File(getClass().getClassLoader().getResource(modsMechCalibAgentProperty.getSlurmScriptFileName()).getPath()), 
				getInputFile(jsonString, jobFolderName), timeStamp);
		if (setUpMsg != null) {
			deleteDirectory(new File(jobFolderPath.concat(".zip")));
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
	 * @throws MoDSMechCalibAgentException
	 */
	private File getInputFile(String jsonString, String jobFolderName) throws IOException, MoDSMechCalibAgentException {
		boolean onlyEval = JSonRequestParser.getIfOnlyEval(jsonString);
		if (!onlyEval) {
			MoDSFileManagement fileMagt = new MoDSFileManagement(modsMechCalibAgentProperty);
			jobFolderPath = fileMagt.createMoDSJob(jsonString, jobFolderName);
		} else {
			MoDS4yrt23 fileMagt = new MoDS4yrt23(modsMechCalibAgentProperty);
			jobFolderPath = fileMagt.createMoDSJob(jsonString, jobFolderName);
		}
		
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
