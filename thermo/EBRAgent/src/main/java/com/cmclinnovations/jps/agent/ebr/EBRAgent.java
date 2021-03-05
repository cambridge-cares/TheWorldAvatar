package com.cmclinnovations.jps.agent.ebr;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;

import java.io.FileOutputStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
import org.apache.commons.collections.map.HashedMap;
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

import com.cmclinnovations.jps.agent.caller.AgentCaller;
import com.cmclinnovations.jps.agent.configuration.EBRAgentProperty;
import com.cmclinnovations.jps.agent.configuration.SpringConfiguration;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.csv.species.CSVGenerator;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;
import com.cmclinnovations.jps.model.species.SpeciesBean;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

/**
 * EBR Agent is developed for setting-up and running thermodata validation<br>
 * and calculation jobs at increasing levels of theory.   
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
	public static ApplicationContext applicationContextSlurmJobAPI;
	public static ApplicationContext applicationContextEBRAgent;
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
     * @param input the JSON input to set up and run an EBR job. The JSON
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
     * @param input the JSON input to set up and run an EBR job.
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
     * Returns the following statistics of EBR jobs processed by the EBR Agent.</br>
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
    public JSONObject produceStatistics(@RequestParam String input) throws IOException, EBRAgentException{
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics(input);
    }
	
	/**
     * Shows the following statistics of EBR jobs processed by EBR Agent.</br>
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
	 * Starts the scheduler to monitor EBR jobs.
	 * 
	 * @throws EBRAgentException
	 */
	public void init() throws ServletException{
        logger.info("---------- EBR Agent (for thermodata validation and calculation) has started ----------");
        System.out.println("---------- EBR Agent (for thermodata validation and calculation) has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        EBRAgent ebrAgent = new EBRAgent();
       	// the first 60 refers to the delay (in seconds) before the job scheduler
        // starts and the second 60 refers to the interval between two consecu-
        // tive executions of the scheduler.
        executorService.scheduleAtFixedRate(() -> {
			try {
				ebrAgent.monitorJobs();
			} catch (SlurmJobException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}, 30, 60, TimeUnit.SECONDS);
		// initialising classes to read properties from the ebr-agent.properites file
		initConfig();
        logger.info("---------- EBR jobs are being monitored  ----------");
        System.out.println("---------- EBR jobs are being monitored  ----------");
       	
	}

	public void initConfig(){
        if (applicationContextSlurmJobAPI == null) {
        	applicationContextSlurmJobAPI = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
        if (applicationContextEBRAgent == null) {
        	applicationContextEBRAgent = new AnnotationConfigApplicationContext(com.cmclinnovations.jps.agent.configuration.SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContextSlurmJobAPI.getBean(SlurmJobProperty.class);
		}
		if (ebrAgentProperty == null) {
			ebrAgentProperty = applicationContextEBRAgent.getBean(EBRAgentProperty.class);
		}
	}
	
	private void monitorJobs() throws SlurmJobException{
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		jobSubmission.monitorJobs();
		processOutputs();
	}
	
	/**
	 * Monitors the currently running EBR jobs to allow new jobs to start.</br>
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
							doDFTCalcsOfInvalidSpecies(jobFolder);
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
		} catch(Exception e){
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
	 * Performs DFT calculations of those species that were evaluated as<br>
	 * holding invalid thermodynamic data.
	 * 
	 * @param jobFolder
	 */
	public void doDFTCalcsOfInvalidSpecies(File jobFolder) throws IOException, Exception{
		File outputFile = new File(jobFolder.getAbsolutePath());
		String zipFilePath = jobFolder.getAbsolutePath().concat(File.separator).concat(slurmJobProperty.getOutputFileName()).concat(slurmJobProperty.getOutputFileExtension());
		zipFilePath = zipFilePath.replaceAll("\\\\", "/");
		String destDir = jobFolder.getAbsolutePath();
		destDir = destDir.replaceAll("\\\\", "/");
//		Utils.unzipFile(zipFilePath, destDir);
//		Utils.unzip(outputFile.getAbsolutePath(), new File(jobFolder.getAbsolutePath()));
		BufferedReader br = Utils.openSourceFile(jobFolder.getAbsolutePath().concat(File.separator)
				.concat(ebrAgentProperty.getEbrAgentInvalidSpeciesFolder()).concat(File.separator).concat(ebrAgentProperty.getEbrAgentInvalidSpeciesFile()));
		String line;
		while((line=br.readLine())!=null){
			if(!line.isEmpty()){
				AgentCaller agentCaller = new AgentCaller();
				String speciesToRunDFTCalculation = ebrAgentProperty.getEbrAgentOntoSpeciesABoxBaseUrl().concat(line).concat(ebrAgentProperty.getEbrAgentOntoSpeciesABoxFileExtension()).concat("#").concat(line);
				logger.info("EBRAgent: sending a request to do DFT calculation of the species:"+speciesToRunDFTCalculation);
				String httpRequest = agentCaller.produceHTTPRequest(speciesToRunDFTCalculation);
				logger.info("EBRAgent: peforming the following http request to DFT Agent:"+httpRequest);
				AgentCaller. performHTTPRequest(httpRequest);
				logger.info("EBRAgent: a DFT job has been submitted.");
			}
		}
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
	 * Sets up an EBR job by creating the job folder and the following files</br>
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
	 * Sets up the EBR job for the current input.
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
	 * Sets up the EBR job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonInput
	 * @return
	 * @throws Exception 
	 * 
	 */
	public File getInputFile(String jsonInput) throws Exception{
		File inputFolder = Utils.createInputFolder(jsonInput);
		LinkedList<SpeciesBean> nistRefSpeciesIdList = new LinkedList<SpeciesBean>();
		LinkedList<SpeciesBean> nistTargetSpeciesIdList = new LinkedList<SpeciesBean>();
		CSVGenerator csvGenerator = new CSVGenerator();
		OntoSpeciesKG oskg = new OntoSpeciesKG();
		List<Map<String, Object>> referenceSpeciesList =  JSonRequestParser.getAllReferenceSpeciesIRI(jsonInput);
		/**
		 * Stores in List<Map> pairs of target species IRIs and corresponding ontospecies IRIs.
		 */
		List<Map<String, Object>> targetSpeciesList =  JSonRequestParser.getAllTargetSpeciesIRI(jsonInput);
		logger.info("reference species size: " + referenceSpeciesList.size());
		logger.info("target species size: " + targetSpeciesList.size());
		nistRefSpeciesIdList = SpeciesBean.getSpeciesIRIList(referenceSpeciesList, nistRefSpeciesIdList, oskg, false);
		nistTargetSpeciesIdList = SpeciesBean.getSpeciesIRIList(targetSpeciesList, nistTargetSpeciesIdList, oskg, true);
		logger.info("reference species size: " + referenceSpeciesList.size());
		LinkedList<String> ontoCompChemIRIList = new LinkedList<String>();
		Map<String, String> compChemVsOntoSpeciesMap = new HashedMap(); 
		populateOntoCompChemIriDataStructures(referenceSpeciesList, ontoCompChemIRIList, compChemVsOntoSpeciesMap);
		populateOntoCompChemIriDataStructures(targetSpeciesList, ontoCompChemIRIList, compChemVsOntoSpeciesMap);
		Map<String, String> gaussianIriVsCompChemMap = new HashedMap(); 
		LinkedList<String> queryResultList = new LinkedList<String>();
		for(String s : ontoCompChemIRIList) {
			logger.info("onto comp chem species iri : " + s);
			List<String> gaussianIRIs = oskg.queryOntoCompChemSpeciesRepository(ebrAgentProperty.getOntoCompChemKBSingleEndPoint(), s); 
			queryResultList.addAll(gaussianIRIs);
			if(gaussianIRIs !=null && gaussianIRIs.size()>0){
				gaussianIriVsCompChemMap.put(gaussianIRIs.get(0), s);
			}
		}
		for(String gaussianIRI : queryResultList) {
			System.out.println("gaussianIRI: " + gaussianIRI);
			String inputSourceGaussianFileOnLocalHost = gaussianIRI;//.replaceAll("http://www.theworldavatar.com/", "http://localhost:8080/");
			String gaussianFileName = inputSourceGaussianFileOnLocalHost.substring(inputSourceGaussianFileOnLocalHost.lastIndexOf("/") + 1);
			String ontoSpeciesIriAsGaussianFileName = getOntoSpeciesIriAsGaussianFileName(compChemVsOntoSpeciesMap.get(gaussianIriVsCompChemMap.get(gaussianIRI)), gaussianFileName);
			/**
			 * Copies all uploaded Gaussian files to input folder on users profile
			 */
			Utils.copyFileFromURL(inputSourceGaussianFileOnLocalHost,  SystemUtils.getUserHome()+"/"+JSonRequestParser.getDFTCalculationPath(jsonInput)+"/" +ontoSpeciesIriAsGaussianFileName);
		}
		csvGenerator.generateCSVFile(nistRefSpeciesIdList, SystemUtils.getUserHome()+"/"+JSonRequestParser.getReferenceSpeciesPool(jsonInput));
		csvGenerator.generateCSVFile(nistTargetSpeciesIdList, SystemUtils.getUserHome()+"/"+JSonRequestParser.getTargetSpeciesPool(jsonInput));
		//Return the input zip file.
		return Utils.getZipFile(inputFolder.getAbsolutePath()); 
	}
	
	/**
	 * Populates a list of map with the key OntoSpecies IRI and value<br>
	 * OntoCompChem IRI, only OntoCompChemIRI and a map with the key<br>
	 * OntoCompChemIRI and the value OntoSpecies IRI. 
	 * 
	 * @param speciesList
	 * @param ontoCompChemIRIList
	 * @param compChemVsOntoSpeciesMap
	 */
	private void populateOntoCompChemIriDataStructures(List<Map<String, Object>> speciesList, LinkedList<String> ontoCompChemIRIList, Map<String, String> compChemVsOntoSpeciesMap){
		for(Map<String, Object> map: speciesList) {
			String ontocompchemIRI = null;
			String ontospeciesIRI = null;
			for (Map.Entry<String, Object> m : map.entrySet()) {
				if (m.getKey().matches("ontocompchemIRI")) {
					ontocompchemIRI = m.getValue().toString();
					ontoCompChemIRIList.add(ontocompchemIRI);
				}
				if(m.getKey().matches("ontospeciesIRI")){
					ontospeciesIRI = m.getValue().toString();
				}
				if(ontocompchemIRI!=null && ontospeciesIRI!=null){
					compChemVsOntoSpeciesMap.put(ontocompchemIRI, ontospeciesIRI);
				}
			}
		}
	}
	
	/**
	 * Link species IDs in reference and target CSV files to Gaussian file<br>
	 * names via UUIDs, which are used as the IRI of species.
	 * 
	 * @param ontoSpeciesIRI
	 * @param gaussianFileName
	 * @return
	 * @throws Exception
	 */
	private String getOntoSpeciesIriAsGaussianFileName(String ontoSpeciesIRI, String gaussianFileName) throws Exception{
		if(!gaussianFileName.contains(".")){
			logger.error("Gaussian file name does not have the extension.");
			throw new Exception("Gaussian file name does not have the extension.");
		}
		String tokens[] = gaussianFileName.split("\\.");
		if(!ontoSpeciesIRI.contains("/")){
			logger.error("OntoSpecies IRI does not contain frontslash(/)");
			throw new Exception("OntoSpecies IRI does not contain frontslash(/)");
		}
		// If the ontospecies IRI is http://www.theworldavatar.com/kb/ontospecies/f2b02ae7-542b-3f63-a81b-af688ae86865.owl#f2b02ae7-542b-3f63-a81b-af688ae86865,
		// it returns the part between last frontslash (/) and .owl, which is f2b02ae7-542b-3f63-a81b-af688ae86865
		return ontoSpeciesIRI.substring(ontoSpeciesIRI.lastIndexOf("/") + 1, ontoSpeciesIRI.lastIndexOf(".")+1).concat(tokens[1]);
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