package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
import org.apache.commons.lang3.StringUtils;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.cmclinnovations.jps.agent.json.parser.AgentRequirementParser;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.kg.OntoAgentKG;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;
import com.cmclinnovations.slurm.job.JobSubmission;
import com.cmclinnovations.slurm.job.SlurmJob;
import com.cmclinnovations.slurm.job.SlurmJobException;
import com.cmclinnovations.slurm.job.Status;
import com.cmclinnovations.slurm.job.configuration.SlurmJobProperty;
import com.cmclinnovations.slurm.job.configuration.SpringConfiguration;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

/**
 * Quantum Calculation Agent developed for setting-up and running quantum
 * jobs at increasing levels of theory.   
 * 
 * @author msff2
 *
 */
@Controller
public class EBRAgent extends HttpServlet{
	
	private Logger logger = LoggerFactory.getLogger(EBRAgent.class);	
	String server = "login-skylake.hpc.cam.ac.uk";
	String username = "msff2";
	String password = "Abcdl955_l7_l7_l7_aB";
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
	
	
	public static void main(String[] args) throws ServletException, EBRAgentException{
		EBRAgent ebrAgent = new EBRAgent();
		ebrAgent.init();
	}
	
	/**
     * Allows to perform a SPARQL query of any complexity.</br>
     * It returns the results in JSON format.
     * 
     * @param input the JSON input to set up and run a quantum job.
     * @return a message if the job was set up successfully or failed. 
     */
	@RequestMapping(value="/job/request", method = RequestMethod.GET)
    @ResponseBody
    public String query(@RequestParam String input) throws IOException, EBRAgentException, SlurmJobException{	
		
		System.out.println("received query:\n"+input);
		
		logger.info("received query:\n"+input);
		
		return setUpJob(input);
    }
	
	
	/**
     * Shows the following statistics of quantum jobs processed by EBR Agent.</br>
     * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @param input the JSON string specifying the return data format, e.g. JSON.
     * @return the statistics in JSON format if requested. 
     */
	@RequestMapping(value="/job/statistics", method = RequestMethod.GET)
    @ResponseBody
    public String produceStatistics(@RequestParam String input) throws IOException, EBRAgentException{
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics(input);
    }
	
	/**
     * Shows the following statistics of quantum jobs processed by EBR Agent.</br>
     * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @return the statistics in HTML format. 
     */
	@RequestMapping(value="/job/show/statistics", method = RequestMethod.GET)
    @ResponseBody
    public String showStatistics() throws IOException, EBRAgentException{
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics();
    }
	
	/**
	 * Starts the scheduler to monitor quantum jobs.
	 * 
	 * @throws EBRAgentException
	 */
	public void init() throws ServletException{
        logger.info("---------- Quantum Calculation Agent has started ----------");
        System.out.println("---------- Quantum Calculation Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        EBRAgent ebrAgent = new EBRAgent();
       	// the first 60 refers to the delay (in seconds) before the job scheduler
        // starts and the second 60 refers to the interval between two consecu-
        // tive executions of the scheduler.
        executorService.scheduleAtFixedRate(ebrAgent::monitorJobs, 30, 60, TimeUnit.SECONDS);
		// initialising classes to read properties from the ebr-agent.properites file
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
        logger.info("---------- Quantum jobs are being monitored  ----------");
        System.out.println("---------- Quantum jobs are being monitored  ----------");
       	
	}
	
	private void monitorJobs(){
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	/**
	 * Monitors the currently running quantum jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * 
	 */
	private void processOutputs() {
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if(jobSpace.isDirectory()){
				File[] jobFolders = jobSpace.listFiles();
				for(File jobFolder: jobFolders){
					if(Utils.isJobCompleted(jobFolder)){
						if(!Utils.isJobOutputProcessed(jobFolder)){
							// Call Upload Service
							updateJobOutputStatus(jobFolder);
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch(SftpException e){
			e.printStackTrace();
		} catch(JSchException e){
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
	private boolean updateJobOutputStatus(File jobFolder)
			throws JSchException, SftpException, IOException, InterruptedException {
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
	private boolean updateJobOutputStatus(String completedJob, File statusFile) throws JSchException, SftpException, IOException, InterruptedException{
		if(statusFile!=null){
			Utils.modifyOutputStatus(statusFile.getAbsolutePath(), Status.OUTPUT_PROCESSED.getName());
			return true;
		}
		return false;
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
	 * @throws EBRAgentException
	 */
	public String setUpJob(String jsonString) throws IOException, EBRAgentException, SlurmJobException{
		
        	String message = setUpJobOnAgentMachine(jsonString);
			JSONObject obj = new JSONObject();
			obj.put("message", message);
			
			logger.info("message:" + message);
			
        	return obj.toString();
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
	 * Sets up the quantum job for the current input.
	 *   
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws EBRAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonInput) throws IOException, EBRAgentException, SlurmJobException {
		
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(),
					slurmJobProperty.getHpcAddress());
		}
		
		
		//jobSubmission.setUpJob(jsonInput, slurmScript, input, executable)
		/**
		 * executable = jar file of comoenthalpyestimatiopaper.jar
		 * input = zip file containting g09 files, generated csv file (target species)
		 * slurmScript
		 * jsonInput = input
		 */
		
		long timeStamp = Utils.getTimeStamp();		
		return jobSubmission.setUpJob(
				jsonInput, new File(getClass().getClassLoader()
						.getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()),
				getInputFile(jsonInput), timeStamp);
	}
	
	
	/**
	 * Sets up the quantum job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonInput
	 * @return
	 * @throws IOException
	 * @throws EBRAgentException
	 * 
	 */
	private File getInputFile(String jsonInput) throws IOException, EBRAgentException{
		
		List<NISTSpeciesId> nistSpeciesIdList = new ArrayList<NISTSpeciesId>();
		
		OntoSpeciesKG oskg = new OntoSpeciesKG();
		
		/**
		 * Feroz's line
		 */
//    	String speciesIRI = JSonRequestParser.getSpeciesIRI(jsonInput);
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 */	
		
		List<Map<String, Object>> species =  JSonRequestParser.getAllSpeciesIRI(jsonInput);
		
		for (Map<String, Object> speciesMap : species) {
		
			LinkedList<String> speciesIRIList = new LinkedList<String>();
			
		for(Map.Entry<String, Object> entry : speciesMap.entrySet()) {
				
		System.out.println("key: " + entry.getKey() + " , value: " + entry.getValue().toString());
		
		speciesIRIList.add(entry.getValue().toString());
			
		}
		
		
		try {
			
			String queryString =oskg.formSpeciesQueryFromJsonInput(speciesIRIList.getFirst(),speciesIRIList.getLast());
			
			nistSpeciesIdList.addAll(oskg.runFederatedQueryRepositories(Property.RDF4J_SERVER_URL_FOR_LOCALHOST_ONTOSPECIES_END_POINT.getPropertyName(), Property.RDF4J_SERVER_URL_FOR_CLAUDIUS_ONTOCOMPCHEM_END_POINT.getPropertyName(),queryString));
			
		} catch (Exception e) {
			
			e.printStackTrace();
		}
		
		
		}		
		
		/**
		 * 
		 * Commented lines is Feroz's code.
		 * 
		 */
//    	String speciesGeometry = oskg.querySpeciesGeometry(speciesIRI);    	
//    	System.out.println("speciesGeometry:  " + speciesGeometry);    
//		if(speciesGeometry == null && speciesGeometry.trim().isEmpty()){
//			throw new EBRAgentException(Status.JOB_SETUP_SPECIES_GEOMETRY_ERROR.getName());
//    	}
		
		/**
		 * 
		 * Comment: "input" foder in users profile is crated before running ebr service. inside that folder we are storing automatically csv file. g09 are copied manually.
		 * DO TO: 
		 * 1. Do federated query (csv)
		 * 2. To create all the content of zip input file.
		 * 
		 */
		
		String inputFilePath = getInputFilePath();	
		
    	return new File(inputFilePath);
	}
	
	/**
	 * Creates the input file for a Slurm job.
	 * 
	 * @param inputFilePath
	 * @param jobFolder
	 * @param geometry
	 * @param jsonString
	 * @return
	 * @throws IOException
	 */
	public String createInputFile(String inputFilePath, String jobFolder, String geometry, String jsonString) throws IOException{
		BufferedWriter inputFile = Utils.openBufferedWriter(inputFilePath);
		if(inputFile == null){
			return null;
		}
		inputFile.write(Property.JOB_NO_OF_CORES_PREFIX.getPropertyName().concat(AgentRequirementParser.getNumberOfCores(OntoAgentKG.getNumberOfCores()).concat("\n")));
		inputFile.write(Property.JOB_MEMORY_PREFIX.getPropertyName().concat(AgentRequirementParser.getRAMSize(OntoAgentKG.getMemorySize()))
				.concat(Property.JOB_MEMORY_UNITS.getPropertyName()).concat("\n"));
		inputFile.write(Property.JOB_CHK_POINT_FILE_PREFIX.getPropertyName().concat(slurmJobProperty.getHpcAddress()).concat("_")
				.concat(getTimeStampPart(jobFolder))
				.concat(slurmJobProperty.getCheckPointFileExtension()).concat("\n"));
		inputFile.write(slurmJobProperty.getJobPreprintDirective().concat(" ")
				.concat(JSonRequestParser.getLevelOfTheory(jsonString)).concat(" ")
				.concat(JSonRequestParser.getJobKeyword(jsonString)).concat(" ")
				.concat(JSonRequestParser.getAlgorithmChoice(jsonString)).concat("\n\n"));
		inputFile.write(" ".concat(jobFolder).concat("\n\n"));
		inputFile.write(Property.SPECIES_CHARGE_ZERO.getPropertyName().concat(" ")
				.concat(Property.SPECIES_MULTIPLICITY.getPropertyName()).concat("\n"));
		inputFile.write(geometry.concat("\n"));
		inputFile.close();
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
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
	public String getTimeStampPart(String folder){
		if(folder.contains("_")){
			String[] tokens = folder.split("_");
			if(tokens.length==2 && tokens[1]!=null && StringUtils.isNumeric(tokens[1])){
				return tokens[1];
			}
		}
		return null;
	}
	
	/**
	 * Produces a job folder name by following the schema hpcAddress_timestamp.
	 * 
	 * @param hpcAddress
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress){
		return hpcAddress.concat("_").concat("" + Utils.getTimeStamp());
	}
	
	/**
	 * Based on the agent workspace directory, input file name and<br>
	 * extension, it can generate the input file path.  
	 * 
	 * @return
	 */
	public String getInputFilePath(){
		return Property.AGENT_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator).concat(slurmJobProperty.getInputFileName())
		.concat(slurmJobProperty.getInputFileExtension());
	}
}
