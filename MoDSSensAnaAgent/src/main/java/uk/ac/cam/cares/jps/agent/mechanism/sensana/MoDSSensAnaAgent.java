package uk.ac.cam.cares.jps.agent.mechanism.sensana;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.apache.commons.lang.StringUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.MoDSSensAnaAgentProperty;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.MoDSFileManagement;
import uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana.MoDSFileMagtSensAna;
import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
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

public class MoDSSensAnaAgent extends HttpServlet {
	private Logger logger = LoggerFactory.getLogger(MoDSSensAnaAgent.class);
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
	static JobSubmission jobSubmission;
	public static ApplicationContext applicationContext;
	public static SlurmJobProperty slurmJobProperty;
	public static ApplicationContext applicationContextMoDSAgent;
	public static MoDSSensAnaAgentProperty modsAgentProperty;
	
	public static void main(String[] args) throws ServletException, MoDSSensAnaAgentException {
		MoDSSensAnaAgent modsSensAnaAgent = new MoDSSensAnaAgent();
		modsSensAnaAgent.init();
//		String input = "{\"json\":{\"ontochemexpIRI\":{\"ignitionDelay\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_404313416274000\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001701.owl#Experiment_404313804188800\",\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001702.owl#Experiment_404313946760600\"],\"flameSpeed\":[\"https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001703.owl#Experiment_2748799135285400\"]},\"ontokinIRI\":{\"reactionList\":[\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570512_48\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570503_39\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570639_175\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570640_176\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570509_45\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570499_35\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570607_143\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570631_167\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570634_170\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570633_169\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570504_40\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570502_38\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570618_154\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570505_41\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570638_174\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570517_53\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570604_140\",\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ChemicalReaction_1230848575570624_160\"],\"mechanism\":\"http://www.theworldavatar.com/kb/ontokin/pode_mechanism_testing.owl#ReactionMechanism_1230848575548237\"}}}";
//		try {
//			modsAgent.query(input);
//			
//		} catch (IOException | SlurmJobException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
	}
	
	/**
	 * Allows to perform a SPARQL query of any complexity. 
	 * It returns the resutls in JSON format. 
	 * 
	 * @param input the JSON input to set up and run a calibration job. 
	 * @return a message if the job was set up successfully or failed.
	 */
	@RequestMapping(value="/job/request", method = RequestMethod.GET)
	@ResponseBody
	public String query(@RequestParam String input) throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		System.out.println("received query:\n"+input);
		logger.info("received query:\n"+input);
		return setUpJob(input);
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
	@RequestMapping(value="/job/statistics", method = RequestMethod.GET)
	@ResponseBody
	public String produceStatistics(@RequestParam String input) throws IOException, MoDSSensAnaAgentException {
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		if (jobSubmission==null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics(input);
	}
	
	/**
	 * Shows the following statistics of quantum jobs processed by MoDS Agent.
	 * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @return the statistics in HTML format.
	 */
	
	@RequestMapping(value="/job/show/statistics",method = RequestMethod.GET)
	@ResponseBody
	public String showStatistics() throws IOException, MoDSSensAnaAgentException {
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		if (jobSubmission==null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics();
	}
	
	/**
	 * Starts the scheduler to monitor calibration jobs.
	 * 
	 * @throws MoDSSensAnaAgentException
	 */
	public void init() throws ServletException {
		logger.info("---------- Mechanism Calibration Agent has started ----------");
		System.out.println("---------- Mechanism Calibration Agent has started ----------");
		ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
		MoDSSensAnaAgent modsAgent = new MoDSSensAnaAgent();
		// the first 30 refers to the delay (in seconds) before the job scheduler
		// starts and the second 60 refers to the interval between two consecutive
		// executions of the scheduler.
		executorService.scheduleAtFixedRate(modsAgent::monitorJobs, 30, 60, TimeUnit.SECONDS);
		// initialising classes to read properties from the mods-agent.properties file
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (applicationContextMoDSAgent == null) {
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(MoDSSensAnaAgentConfiguration.class);
		}
		if (modsAgentProperty == null) {
			modsAgentProperty = applicationContextMoDSAgent.getBean(MoDSSensAnaAgentProperty.class);
		}
		logger.info("---------- Calibration jobs are being monitored  ----------");
		System.out.println("---------- Calibration jobs are being monitored  ----------");
	}
	
	private void monitorJobs() {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		try {
			jobSubmission.monitorJobs();
		} catch (SlurmJobException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// enable process outputs later on
//		processOutputs();
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
			applicationContextMoDSAgent = new AnnotationConfigApplicationContext(MoDSSensAnaAgentConfiguration.class);
		}
		if (modsAgentProperty == null) {
			modsAgentProperty = applicationContextMoDSAgent.getBean(MoDSSensAnaAgentProperty.class);
		}
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							//updateJobOutputStatus(jobFolder);
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		} catch (SftpException e) {
//			e.printStackTrace();
//		} catch (JSchException e) {
//			e.printStackTrace();
//		}
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
	public String setUpJob(String jsonString) throws IOException, MoDSSensAnaAgentException, SlurmJobException {
		String message = setUpJobOnAgentMachine(jsonString);
		JSONObject obj = new JSONObject();
		obj.put("message", message);
		return obj.toString();
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
	 * @throws MoDSSensAnaAgentException
	 */
	private File getInputFile(String jsonString, String jobFolderName) throws IOException, MoDSSensAnaAgentException {
		MoDSFileMagtSensAna fileMagt = new MoDSFileMagtSensAna();
		
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
