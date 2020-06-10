package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
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

import com.cmclinnovations.jps.agent.configuration.DFTAgentConfiguration;
import com.cmclinnovations.jps.agent.configuration.DFTAgentProperty;
import com.cmclinnovations.jps.agent.json.parser.AgentRequirementParser;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;
import com.cmclinnovations.jps.kg.OntoAgentKG;
import com.cmclinnovations.jps.kg.OntoKinKG;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;
import com.cmclinnovations.jps.upload.CompChemUpload;
import com.jayway.jsonpath.JsonPath;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

import net.minidev.json.JSONArray;
import uk.ac.cam.cares.jps.base.slurm.job.JobSubmission;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJob;
import uk.ac.cam.cares.jps.base.slurm.job.SlurmJobException;
import uk.ac.cam.cares.jps.base.slurm.job.Status;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;
import uk.ac.cam.cares.jps.base.slurm.job.configuration.SpringConfiguration;

/**
 * Quantum Calculation Agent developed for setting-up and running quantum
 * jobs at increasing levels of theory.   
 * 
 * @author msff2
 *
 */
@Controller
public class DFTAgent extends HttpServlet{
	private Logger logger = LoggerFactory.getLogger(DFTAgent.class);	
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
	public static ApplicationContext applicationContextDFTAgent;
	public static DFTAgentProperty dftAgentProperty;
	
	
	public static void main(String[] args) throws ServletException, DFTAgentException{
		DFTAgent dftAgent = new DFTAgent();
		dftAgent.init();
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
    public String query(@RequestParam String input) throws IOException, DFTAgentException, SlurmJobException{
		System.out.println("received query:\n"+input);
		logger.info("received query:\n"+input);
		return setUpJob(input);
    }
	
	
	/**
     * Shows the following statistics of quantum jobs processed by DFT Agent.</br>
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
    public String produceStatistics(@RequestParam String input) throws IOException, DFTAgentException{
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		if(jobSubmission==null){
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		return jobSubmission.getStatistics(input);
    }
	
	/**
     * Shows the following statistics of quantum jobs processed by DFT Agent.</br>
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
    public String showStatistics() throws IOException, DFTAgentException{
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
	 * @throws DFTAgentException
	 */
	public void init() throws ServletException{
        logger.info("---------- Quantum Calculation Agent has started ----------");
        System.out.println("---------- Quantum Calculation Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        DFTAgent dftAgent = new DFTAgent();
       	// the first 60 refers to the delay (in seconds) before the job scheduler
        // starts and the second 60 refers to the interval between two consecu-
        // tive executions of the scheduler.
        executorService.scheduleAtFixedRate(dftAgent::monitorJobs, 30, 60, TimeUnit.SECONDS);
		// initialising classes to read properties from the dft-agent.properites file
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
	    if (applicationContextDFTAgent == null) {
	    	applicationContextDFTAgent = new AnnotationConfigApplicationContext(DFTAgentConfiguration.class);
		}		
		if (dftAgentProperty == null) {
			dftAgentProperty = applicationContextDFTAgent.getBean(DFTAgentProperty.class);
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
	public void processOutputs() {
        if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (slurmJobProperty == null) {
			slurmJobProperty = applicationContext.getBean(SlurmJobProperty.class);
		}
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(), slurmJobProperty.getHpcAddress());
		}
		if (applicationContextDFTAgent == null) {
	    	applicationContextDFTAgent = new AnnotationConfigApplicationContext(DFTAgentConfiguration.class);
		}		
		if (dftAgentProperty == null) {
			dftAgentProperty = applicationContextDFTAgent.getBean(DFTAgentProperty.class);
		}
		jobSpace = jobSubmission.getWorkspaceDirectory();
		try {
			if (jobSpace.isDirectory()) {
				File[] jobFolders = jobSpace.listFiles();
				for (File jobFolder : jobFolders) {
					if (Utils.isJobCompleted(jobFolder)) {
						if (!Utils.isJobOutputProcessed(jobFolder)) {
							// Calls Upload Service
							String uuid = UploadLogFile(jobFolder);
							// The successful completion of the log file upload
							// triggers the job status update.
							if (uuid != null) {
								updateJobOutputStatus(jobFolder);
								runThermodataAgent(jobFolder, uuid);
							}
						}
					}
				}
			}
		} catch (IOException e) {
			logger.error("DFTAgent: IOException.".concat(e.getMessage()));
			e.printStackTrace();
		} catch (InterruptedException e) {
			logger.error("DFTAgent: InterruptedException.".concat(e.getMessage()));
			e.printStackTrace();
		} catch (SftpException e) {
			logger.error("DFTAgent: SftpException.".concat(e.getMessage()));
			e.printStackTrace();
		} catch (JSchException e) {
			logger.error("DFTAgent: JSchException.".concat(e.getMessage()));
			e.printStackTrace();
		}
	}
	
	/**
	 * Invokes Thermodata Agent to generate thermodata and update the<br>
	 * OntoKin knowledge graph.  
	 * 
	 * @param jobFolder
	 * @param uuid
	 * @throws IOException
	 */
	private void runThermodataAgent(File jobFolder, String uuid) throws IOException{
		boolean isInvokingThermodataAgentRequired = Utils.isInvokingThermoAgentRequired(jobFolder, slurmJobProperty);
		if(isInvokingThermodataAgentRequired){
			String result = invokeThermodataAgent(jobFolder, Property.ONTOCOMPCHEM_KB_IRI.getPropertyName()
					.concat(uuid).concat("/").concat(uuid).concat(".owl#").concat(uuid));
			boolean isApplyingThermoUpdateToMechanismRequired = Utils.isApplyingThermoUpdateToMechanismRequired(jobFolder, slurmJobProperty);
			if(isApplyingThermoUpdateToMechanismRequired && !result.isEmpty()){
				String uniqueSpeciesIRI = Utils.getUniqueSpeciesIRI(jobFolder, slurmJobProperty);
				JSONArray lowTCoeffArray = JSonRequestParser.getLowTemperatureCoefficient(result);
				String lowTCoeff = formatCoefficient(lowTCoeffArray);
				JSONArray highTCoeffArray = JSonRequestParser.getHighTemperatureCoefficient(result);
				String highTCoeff = formatCoefficient(highTCoeffArray);
				List<String> thermoModelModelTriples = queryThermoModelFromOntoKinKG(uniqueSpeciesIRI);
				updateThermoModelInOntoKinKG(thermoModelModelTriples, lowTCoeff, highTCoeff);
			}
		}
	}
	
	/**
	 * Queries and retrieves a thermo model of the species from the OntoKin KG. 
	 * 
	 * @param uniqueSpeciesIRI
	 * @return
	 * @throws IOException
	 */
	private List<String> queryThermoModelFromOntoKinKG(String uniqueSpeciesIRI) throws IOException{
		String thermoModelsSPARQLQueryOutputInJson = OntoKinKG.getThermoModelsOfSpecies(uniqueSpeciesIRI, dftAgentProperty);
		JSONArray thermoModels = JSonRequestParser.getThermoModels(thermoModelsSPARQLQueryOutputInJson);
		return getSortedThermoModelTriples(thermoModels, thermoModelsSPARQLQueryOutputInJson);
	}
	
	/**
	 * Updates a thermo model of the species in the OntoKin KG.
	 * 
	 * @param thermoModelModelTriples
	 * @param lowTCoeff
	 * @param highTCoeff
	 * @throws IOException
	 */
	private void updateThermoModelInOntoKinKG(List<String> thermoModelModelTriples, String lowTCoeff, String highTCoeff) throws IOException{
		int iteration = 0;
		for(String thermoModelTriple:thermoModelModelTriples){
			OntoKinKG.deleteThermoModelsOfSpecies(thermoModelTriple, dftAgentProperty);
			String tokens[] = thermoModelTriple.split(" ");
			String newThermoModelTriple = "";
			for(int i=0;i<tokens.length;i++){
				if(i<2){
					newThermoModelTriple = newThermoModelTriple + tokens[i] + " ";
				}else{
					break;
				}
			}
			if(iteration == 0){
				newThermoModelTriple = newThermoModelTriple + lowTCoeff;
			}else if(iteration == 1){
				newThermoModelTriple = newThermoModelTriple + highTCoeff;
			}
			iteration++;
			OntoKinKG.insertThermoModelsOfSpecies(newThermoModelTriple, dftAgentProperty);
		}
	}
	
	
	
	/**
	 * Puts retrieved thermo models in triple format and srot them.
	 * 
	 * @param thermoModels
	 * @param jsonString
	 * @return
	 */
	private List<String> getSortedThermoModelTriples(JSONArray thermoModels, String jsonString) {
		List<String> thermoModelTriples = new ArrayList<>();
		for (int i = 0; i < thermoModels.size(); i++) {
			thermoModelTriples.add(JsonPath.read(jsonString, "$.results.bindings[" + i + "].thermoModel.value")
					.toString().concat(" ").concat("http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasCoefficientValues").concat(" ")
					.concat(JsonPath.read(jsonString, "$.results.bindings[" + i + "].coeffValues.value").toString()));
		}
		Collections.sort(thermoModelTriples);
		return thermoModelTriples;
	}
	
	/**
	 * Formatted NASA Coefficients to align the representation with the<br>
	 * CTML converter. 
	 * 
	 * @param coeff
	 * @return
	 */
	private String formatCoefficient(JSONArray coeffArray){
		String formattedCoeff = "\n";
		for(int i=0;i<coeffArray.size();i++){
			if(i>=6){
				formattedCoeff = formattedCoeff.concat(coeffArray.get(i).toString());
				break;
			}else if(i==3){
				formattedCoeff = formattedCoeff.concat(coeffArray.get(i).toString()).concat(",\n");					
			}else{
				formattedCoeff = formattedCoeff.concat(coeffArray.get(i).toString()).concat(",   ");
			}
		}
		return formattedCoeff;
	}
	
	/**
	 * Uploads the log file to the OntoCompChem knowledge graph. If the<br>
	 * upload is successful, it returns true. If the upload fails, it<br>
	 * throws an exception. If the log file does not exist, for any<br>
	 * reason, it returns false.
	 * 
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	private String UploadLogFile(File jobFolder) throws IOException{
		File logFile = new File(jobFolder.getAbsolutePath().concat(File.separator).concat(jobFolder.getName()).concat(slurmJobProperty.getOutputFileExtension()));
		String uploadMessage = "";
		if(logFile.exists()){
			String uniqueSpeciesIRI = Utils.getUniqueSpeciesIRI(jobFolder, slurmJobProperty);
			try{
				CompChemUpload compChemUpload = new CompChemUpload();
				compChemUpload.setCalculationFileName(logFile.getName());
				compChemUpload.setCalculationFilePath(logFile.getAbsolutePath());
				compChemUpload.setOntoSpeciesIRI(uniqueSpeciesIRI);
				uploadMessage = compChemUpload.upload();
				}catch(Exception e){
					return null;
				}
			return uploadMessage;
		}else{
			return null;
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
	
	private String invokeThermodataAgent(File jobFolder, String ontoCompChemIRI) throws IOException{
		JSONObject json = new JSONObject();
		json.put("gaussian", ontoCompChemIRI);
		String httpRequest = dftAgentProperty.getThermoAgentHttpRequestFirstPart().concat(URLEncoder.encode(json.toString(), "UTF-8"));
		int limit = 5;
		String result = "";
		for(int i = 1; i<=5; i++){
			try{
				result = performHTTPRequest(httpRequest);
				break;
			}catch(IOException e){
				logger.info("Attempt["+i+"] Could not receive the calculated NASA polynomials.");
			}
		}
		return result;
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
	 * @throws DFTAgentException
	 */
	public String setUpJob(String jsonString) throws IOException, DFTAgentException, SlurmJobException{
        	String message = setUpJobOnAgentMachine(jsonString);
			JSONObject obj = new JSONObject();
			obj.put("message", message);
        	return obj.toString();
    }
	
	/**
	 * Sets up the quantum job for the current input.
	 *   
	 * @param jsonInput
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonInput) throws IOException, DFTAgentException, SlurmJobException {
		if (jobSubmission == null) {
			jobSubmission = new JobSubmission(slurmJobProperty.getAgentClass(),
					slurmJobProperty.getHpcAddress());
		}
		long timeStamp = Utils.getTimeStamp();
		String jobFolderName = getNewJobFolderName(slurmJobProperty.getHpcAddress(), timeStamp);
		return jobSubmission.setUpJob(
				jsonInput, new File(getClass().getClassLoader()
						.getResource(slurmJobProperty.getSlurmScriptFileName()).getPath()),
				getInputFile(jsonInput, jobFolderName), timeStamp);
	}
	
	/**
	 * Sets up the quantum job for the current request.
	 * 
	 * @param jsonInput
	 * @param jobFolderName
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private File getInputFile(String jsonInput, String jobFolderName) throws IOException, DFTAgentException{
		OntoSpeciesKG oskg = new OntoSpeciesKG(); 
    	String speciesIRI = JSonRequestParser.getSpeciesIRI(jsonInput);
    	if(speciesIRI == null && speciesIRI.trim().isEmpty()){
    		throw new DFTAgentException(Status.JOB_SETUP_SPECIES_IRI_MISSING.getName());
    	}
		String speciesGeometry = oskg.querySpeciesGeometry(speciesIRI, slurmJobProperty);
		if(speciesGeometry == null && speciesGeometry.trim().isEmpty()){
			throw new DFTAgentException(Status.JOB_SETUP_SPECIES_GEOMETRY_ERROR.getName());
    	}
		String inputFilePath = getInputFilePath();
		String inputFileMsg = createInputFile(inputFilePath, jobFolderName, speciesGeometry, jsonInput);
		if(inputFileMsg == null){
			throw new DFTAgentException(Status.JOB_SETUP_INPUT_FILE_ERROR.getName());
		}
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
	 * @param timeStamp
	 * @return
	 */
	public String getNewJobFolderName(String hpcAddress, long timeStamp){
		return hpcAddress.concat("_").concat("" + timeStamp);
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
	 * Enables to perform an HTTP get request.
	 * 
	 * @param query
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public static String performHTTPRequest(String query) throws MalformedURLException, IOException{
        URL httpURL = new URL(query);
        URLConnection httpURLConnection = httpURL.openConnection();
        BufferedReader in = new BufferedReader(
                                new InputStreamReader(
                                		httpURLConnection.getInputStream()));
        String inputLine;
        String fileContent = "";
        while ((inputLine = in.readLine()) != null){ 
            fileContent = fileContent.concat(inputLine);
        }
        in.close();
        System.out.println("fileContent:\n"+fileContent);
        return fileContent;
    }
}
