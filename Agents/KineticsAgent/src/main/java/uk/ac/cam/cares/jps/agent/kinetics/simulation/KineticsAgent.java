package uk.ac.cam.cares.jps.agent.kinetics.simulation;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.slf4j.LoggerFactory;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.KineticsAgentProperty;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.PostProcessing;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.Utils;

/**
 * Kinetics Agent developed for setting-up and running kinetics simulation jobs.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Controller
@WebServlet(urlPatterns = { KineticsAgent.JOB_REQUEST_PATH, KineticsAgent.JOB_STATISTICS_PATH,
		KineticsAgent.JOB_OUTPUT_REQUEST_PATH })
public class KineticsAgent extends JPSAgent{
	private static final long serialVersionUID = -8669607645910441935L;
	private Logger logger = LoggerFactory.getLogger(KineticsAgent.class);	
	private File workspace;
	static JobSubmission jobSubmission;
	public static ApplicationContext applicationContextKineticsAgent;
	public static KineticsAgentProperty kineticsAgentProperty;
	
	public static final String BAD_REQUEST_MESSAGE_KEY = "message";
	public static final String UNKNOWN_REQUEST = "The request is unknown to Kinetics Agent";
	
    public static final String JOB_REQUEST_PATH = "/job/request";
    public static final String JOB_OUTPUT_REQUEST_PATH = "/job/output/request";
    public static final String JOB_STATISTICS_PATH = "/job/statistics";
    public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";
	
	/**
     * Shows the following statistics of quantum jobs processed by Kinetics Agent.</br>
     * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @param input the JSON string specifying the return data format, e.g. JSON.
     * @return the statistics in JSON format if requested. 
     */
    public JSONObject produceStatistics(String input) throws IOException, KineticsAgentException{
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		// Initialises all properties required for this agent to set-up<br>
		// and run jobs. It will also initialise the unique instance of<br>
		// Job Submission class.
		initAgentProperty();
		return jobSubmission.getStatistics(input);
    }
	
	/**
     * Shows the following statistics of quantum jobs processed by Kinetics Agent.<br>
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
	@RequestMapping(value=KineticsAgent.JOB_SHOW_STATISTICS_PATH, method = RequestMethod.GET)
    @ResponseBody
    public String showStatistics() throws IOException, KineticsAgentException{
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		initAgentProperty();
		return jobSubmission.getStatistics();
    }
	
	/**
	 * Starts the asynchronous scheduler to monitor quantum jobs.
	 * 
	 * @throws KineticsAgentException
	 */
	public void init() throws ServletException{
        logger.info("---------- Quantum Calculation Agent has started ----------");
        System.out.println("---------- Quantum Calculation Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        KineticsAgent kineticsAgent = new KineticsAgent();
		// initialising classes to read properties from the kinetics-agent.properites file
        initAgentProperty();
		// In the following method call, the parameter getAgentInitialDelay-<br>
		// ToStartJobMonitoring refers to the delay (in seconds) before<br>
		// the job scheduler starts and getAgentPeriodicActionInterval<br>
		// refers to the interval between two consecutive executions of<br>
		// the scheduler.
		executorService.scheduleAtFixedRate(() -> {
			try {
				kineticsAgent.monitorJobs();
			} catch (SlurmJobException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}, kineticsAgentProperty.getAgentInitialDelayToStartJobMonitoring(),
				kineticsAgentProperty.getAgentPeriodicActionInterval(), TimeUnit.SECONDS);
		logger.info("---------- Quantum jobs are being monitored  ----------");
        System.out.println("---------- Quantum jobs are being monitored  ----------");
       	
	}
	
	/**
	 * Initialises the unique instance of the KineticsAgentProperty class that<br>
	 * reads all properties of KineticsAgent from the kinetics-agent property file.<br>
	 * 
	 * Initialises the unique instance of the SlurmJobProperty class and<br>
	 * sets all properties by reading them from the kinetics-agent property file<br>
	 * through the KineticsAgent class.
	 */
	private void initAgentProperty() {
		// initialising classes to read properties from the kinetics-agent.properites
		// file
		if (applicationContextKineticsAgent == null) {
			applicationContextKineticsAgent = new AnnotationConfigApplicationContext(KineticsAgentConfiguration.class);
		}
		if (kineticsAgentProperty == null) {
			kineticsAgentProperty = applicationContextKineticsAgent.getBean(KineticsAgentProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(kineticsAgentProperty.getAgentClass(), kineticsAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setHpcServerLoginUserName(kineticsAgentProperty.getHpcServerLoginUserName());
			jobSubmission.slurmJobProperty
					.setHpcServerLoginUserPassword(kineticsAgentProperty.getHpcServerLoginUserPassword());
			jobSubmission.slurmJobProperty.setAgentClass(kineticsAgentProperty.getAgentClass());
			jobSubmission.slurmJobProperty
					.setAgentCompletedJobsSpacePrefix(kineticsAgentProperty.getAgentCompletedJobsSpacePrefix());
			jobSubmission.slurmJobProperty
					.setAgentFailedJobsSpacePrefix(kineticsAgentProperty.getAgentFailedJobsSpacePrefix());
			jobSubmission.slurmJobProperty.setHpcAddress(kineticsAgentProperty.getHpcAddress());
			jobSubmission.slurmJobProperty.setInputFileName(kineticsAgentProperty.getInputFileName());
			jobSubmission.slurmJobProperty.setInputFileExtension(kineticsAgentProperty.getInputFileExtension());
			jobSubmission.slurmJobProperty.setOutputFileName(kineticsAgentProperty.getOutputFileName());
			jobSubmission.slurmJobProperty.setOutputFileExtension(kineticsAgentProperty.getOutputFileExtension());
			jobSubmission.slurmJobProperty.setJsonInputFileName(kineticsAgentProperty.getJsonInputFileName());
			jobSubmission.slurmJobProperty.setJsonFileExtension(kineticsAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setJsonFileExtension(kineticsAgentProperty.getJsonFileExtension());
			jobSubmission.slurmJobProperty.setSlurmScriptFileName(kineticsAgentProperty.getSlurmScriptFileName());
			jobSubmission.slurmJobProperty.setMaxNumberOfHPCJobs(kineticsAgentProperty.getMaxNumberOfHPCJobs());
			jobSubmission.slurmJobProperty.setAgentInitialDelayToStartJobMonitoring(
					kineticsAgentProperty.getAgentInitialDelayToStartJobMonitoring());
			jobSubmission.slurmJobProperty
					.setAgentPeriodicActionInterval(kineticsAgentProperty.getAgentPeriodicActionInterval());
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
		if(path.equals(KineticsAgent.JOB_REQUEST_PATH)) {
			try{
				validateInput(requestParams);
			}catch(BadRequestException e){
				return requestParams.put(BAD_REQUEST_MESSAGE_KEY, e.getMessage());
			}
			try{
			   return setUpJob(requestParams.toString());
			}catch(SlurmJobException | IOException | KineticsAgentException e){
				throw new JPSRuntimeException(e.getMessage());
			}
		} 
//		else if (path.equals(KineticsAgent.JOB_OUTPUT_REQUEST_PATH)){
//			return simulationResults();
//		} 
		else if (path.equals(KineticsAgent.JOB_STATISTICS_PATH)) {
			try {
				return produceStatistics(requestParams.toString());
			} catch (IOException | KineticsAgentException e) {
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
     * Monitors already set up jobs.
     * 
     * @throws SlurmJobException
     */
	private void monitorJobs() throws SlurmJobException{
		//Configures all properties required for setting-up and running a Slurm job. 
		initAgentProperty();
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	/**
	 * Monitors the currently running quantum jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * 
	 */
	public void processOutputs() {
		initAgentProperty();
		workspace = jobSubmission.getWorkspaceDirectory();
		try {
			if (workspace.isDirectory()) {
				File[] jobFolders = workspace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							boolean successful = false;
							/************************************************
							 * [MICHAEL, CALL POST-PROCESSING METHOD HERE AND
							 * INDICATE USING THE successful VARIABLE DECLARED
							 * ABOVE IF THE PROCESS EXECUTED SUCCESSFULLY OR NOT.]
							 ***********************************************/
							// The successful completion of post-processing
							// triggers the job status update.
							if (successful) {
								PostProcessing.updateJobOutputStatus(jobFolder);
							}else{
								Utils.modifyStatus(Utils.getStatusFile(jobFolder).getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
							}
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("KineticsAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		} 
	}
	
	/**
	 * Sets up a quantum job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br.
	 * - the Status file.</br>
	 * - the JSON input file, which comes from the user request.</br>
	 * 
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws KineticsAgentException
	 */
	public JSONObject setUpJob(String jsonString) throws IOException, KineticsAgentException, SlurmJobException{
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
	 * @throws KineticsAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonInput) throws IOException, KineticsAgentException, SlurmJobException {
		initAgentProperty();
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(kineticsAgentProperty.getHpcAddress(), timeStamp);
		return jobSubmission.setUpJob(
				jsonInput, new File(getClass().getClassLoader()
						.getResource(kineticsAgentProperty.getSlurmScriptFileName()).getPath()),
				getInputFile(jsonInput, jobFolderName), timeStamp);
	}
	
	/**
	 * Prepares input files, bundle them in a zip file and return the zip file to the calling method.
	 * 
	 * @param jsonInput
	 * @param jobFolderName
	 * @return
	 * @throws IOException
	 * @throws KineticsAgentException
	 */
	private File getInputFile(String jsonInput, String jobFolderName) throws IOException, KineticsAgentException{
		/************************************************************************************************************************
		 * [MICHAEL, PREPARE INPUT FILES IN A TEMPORARY LOCATION, COMPRESS THEM IN A ZIP FILE AND PUT THE PATH TO THE ZIP FILE BELOW
		 * AT THE PLACE INDICATED]
		 ************************************************************************************************************************/
		return new File("path to the zip file");
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
}
