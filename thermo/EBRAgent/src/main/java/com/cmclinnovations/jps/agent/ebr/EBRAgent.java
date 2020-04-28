package com.cmclinnovations.jps.agent.ebr;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;

import java.io.FileOutputStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
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

import com.cmclinnovations.jps.agent.configuration.EBRAgentProperty;
import com.cmclinnovations.jps.agent.json.parser.AgentRequirementParser;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.csv.species.CSVGenerator;
import com.cmclinnovations.jps.kg.OntoAgentKG;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;
import com.cmclinnovations.jps.model.species.SpeciesBean;
import com.cmclinnovations.slurm.job.JobSubmission;
import com.cmclinnovations.slurm.job.SlurmJob;

import com.cmclinnovations.slurm.job.Status;
import com.cmclinnovations.slurm.job.configuration.SlurmJobProperty;
import com.cmclinnovations.slurm.job.configuration.SpringConfiguration;
import com.jayway.jsonpath.JsonPath;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

/**
 * Quantum Calculation Agent developed for setting-up and running quantum
 * jobs at increasing levels of theory.   
 * 
 * @author msff2
 *
 */
@Controller
public class EBRAgent extends HttpServlet{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
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
	public static EBRAgentProperty ebrAgentProperty;
	
	
	public static void main(String[] args) throws ServletException, EBRAgentException{
		EBRAgent ebrAgent = new EBRAgent();
		ebrAgent.init();
	}
	
	/**
	 * 
	 * @author msff2@cam.ac.uk
	 * 
	 * The is modified by @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
     * Allows to perform a SPARQL query of any complexity.</br>
     * JSON content contains a file path to input JSON file.
     * 
     * @param input the JSON input to set up and run a quantum job. The JSON
     * @return a message if the job was set up successfully or failed. 
	 * @throws Exception
	 *  
     */
	@RequestMapping(value="/job/request/file", method = RequestMethod.GET)
    @ResponseBody
    public String queryViaFile(@RequestParam String input) throws Exception{	
		
		System.out.println("received query:\n"+input);
		
		/**
		 * 
		 * Extract the content of that file to String
		 * Check whether the file paht exists.
		 * Read content of the file and put it into 'input' String
		 *  
		 */
		
		logger.info("received query:\n"+input);
		
		return setUpJob(input);
    }
	
	/**
     * Allows to perform a SPARQL query of any complexity.</br>
     * It returns the results in JSON format.
     * 
     * @param input the JSON input to set up and run a quantum job.
     * @return a message if the job was set up successfully or failed. 
	 * @throws Exception 
     */
	@RequestMapping(value="/job/request", method = RequestMethod.GET)
    @ResponseBody
    public String query(@RequestParam String input) throws Exception{	
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
		if (ebrAgentProperty == null) {
			ebrAgentProperty = applicationContext.getBean(EBRAgentProperty.class);
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
	 * @throws Exception 
	 */
	public String setUpJob(String jsonString) throws Exception{
		
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
	 * @throws Exception 
	 */
	private String setUpJobOnAgentMachine(String jsonInput) throws Exception {
		
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(),
					slurmJobProperty.getHpcAddress());
		}
		long timeStamp = Utils.getTimeStamp();	
		return jobSubmission.setUpJob(jsonInput, new File(getClass().getClassLoader()
				.getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()), getInputFile(jsonInput), new File(getClass().getClassLoader()
						.getResource(Property.EBR_EXECUTABLE.getPropertyName()).getPath()),timeStamp);
	}	
	
	/**
	 * Sets up the quantum job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonInput
	 * @return
	 * @throws Exception 
	 * 
	 */
	
	private File getInputFile(String jsonInput) throws Exception{
		
		Utils.createInputFolder(jsonInput);
		
		LinkedList<SpeciesBean> nistSpeciesIdList = new LinkedList<SpeciesBean>();
		
		CSVGenerator csvGenerator = new CSVGenerator();
		
		OntoSpeciesKG oskg = new OntoSpeciesKG();
		
		/**
		 * Feroz's line
		 */
//    	String speciesIRI = JSonRequestParser.getSpeciesIRI(jsonInput);
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * 
		 * Stores in List<Map> pairs of target species IRIs and corresponding ontospecies IRIs.
		 * 
		 */	
		
		List<Map<String, Object>> targetSpeciesList =  JSonRequestParser.getAllTargetSpeciesIRI(jsonInput);
		
		System.out.println("target species size: " + targetSpeciesList.size());

		nistSpeciesIdList = SpeciesBean.getSpeciesIRIList(targetSpeciesList, nistSpeciesIdList, oskg);
		
		List<Map<String, Object>> referenceSpeciesList =  JSonRequestParser.getAllReferenceSpeciesIRI(jsonInput);
		
		System.out.println("reference species size: " + referenceSpeciesList.size());
		
		for(Map<String, Object> map: referenceSpeciesList) {
			
			LinkedList<String> ontoCompChemIRIList = new LinkedList<String>();
			
			for(Map.Entry<String, Object> m : map.entrySet()) {
				
				if(m.getKey().matches("ontocompchemIRI")) {
				
				System.out.println("m.getKey(): " + m.getKey());
				System.out.println("m.getValue(): " + m.getValue());
				
				String speciesIRI =m.getValue().toString();
					
				ontoCompChemIRIList.add(speciesIRI);
				
				} else {
					
					continue;
				}
			}
			
			LinkedList<String> queryResultList = new LinkedList<String>();
			
			for(String s : ontoCompChemIRIList) {
				
				System.out.println("onto comp chem species iri : " + s);
				
				queryResultList.addAll(oskg.queryOntoCompChemSpeciesRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST_ONTOCOMPCHEM_END_POINT.getPropertyName(), s));
			}
			
			for(String gaussianIRI : queryResultList) {
				
				System.out.println("gaussianIRI: " + gaussianIRI);
				
				String inputSourceGaussianFileOnLocalHost = gaussianIRI.replaceAll("http://www.theworldavatar.com/", "http://localhost:8080/");

				String gaussianFileName = inputSourceGaussianFileOnLocalHost.substring(inputSourceGaussianFileOnLocalHost.lastIndexOf("/") + 1);
				
				/**
				 * Copies all uploaded Gaussian files to input folder on users profile
				 */
//				String inputFolderPath =JSonRequestParser.getGaussianFolderPath(jsonInput);
				
//				String[] tokens = inputFolderPath.split("/");
//				
//				System.out.println(tokens[0]);
//				
//				if(new File(SystemUtils.getUserHome()+"/"+tokens[0]).exists()) {
//					
//					System.out.println(SystemUtils.getUserHome()+File.separator+tokens[0]);
//					
//					FileUtils.deleteDirectory(new File(SystemUtils.getUserHome()+File.separator+tokens[0]));
//				}
//				
//				new File(SystemUtils.getUserHome()+File.separator+tokens[0]).mkdir();
//				new File(SystemUtils.getUserHome()+File.separator+inputFolderPath).mkdir();
				
				
				Utils.copyFileFromURL(inputSourceGaussianFileOnLocalHost,  SystemUtils.getUserHome()+"/"+JSonRequestParser.getDFTCalculationPath(jsonInput)+"/" +gaussianFileName);
			}
		}
		
		csvGenerator.generateCSVFile(nistSpeciesIdList, SystemUtils.getUserHome()+"/"+JSonRequestParser.getReferenceSpeciesPool(jsonInput));
		
		/**
		 * 
		 * Commented lines is Feroz's code.
		 * 
		 */
		
//    	String speciesGeometry = oskg.querySpeciesGeometry(speciesIRI);   
//    	System.out.println("speciesGeometry:  " + speciesGeometry);    
//		if(speciesGeometry == null && speciesGeometry.trim().isEmpty()){
//		throw new EBRAgentException(Status.JOB_SETUP_SPECIES_GEOMETRY_ERROR.getName());
//    	}
		
		
	/**
	 * Feroz line	
	 */
//		String inputFilePath = getInputFilePath();	
		
    	return new File(SystemUtils.getUserHome()+"/"+JSonRequestParser.getInputZipFile(jsonInput)); //here should be absolute file path of the zip file. 
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
	 * 
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
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param inputFolderPath the folder path where Gaussian file will be copied. It is input folder on user's profile.
	 * @param serverFolderName the folder name on sarver where Gaussian file is uploaded.
	 * @param serverUrl the server url. For example, localhost or Claudius.
	 * @throws IOException
	 * 
	 */
	public void copyGaussianFile(List<Map<String, Object>> referenceSpeciesList, String inputFolderPathJson,  String referenceSpeciesFilePathIRI ) throws IOException {

		
		
		InputStream is = null;
        OutputStream os = null;
        
        try {
            is = new FileInputStream(referenceSpeciesFilePathIRI);
            os = new FileOutputStream(inputFolderPathJson);

            // buffer size 2K
            byte[] buf = new byte[2048];

            int bytesRead;
            while ((bytesRead = is.read(buf)) > 0) {
                os.write(buf, 0, bytesRead);
            }
        } finally {
            is.close();
            os.close();
        }
	}
}