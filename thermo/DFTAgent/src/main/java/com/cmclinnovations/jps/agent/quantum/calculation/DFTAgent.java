package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.cmclinnovations.jps.agent.job.request.parser.JSonRequestParser;
import com.cmclinnovations.jps.agent.workspace.management.Workspace;
import com.cmclinnovations.jps.kg.OntoSpeciesKG;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.SftpException;

import ch.ethz.ssh2.Connection;
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
	Connection connection;
	boolean isAuthenticated;
	private File jobSpace;
	
	static com.jcraft.jsch.Session session;
	static JSch jsch = new JSch();
	
	static int scheduledIteration = 0;
	
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
    public String query(@RequestParam String input) throws IOException, DFTAgentException{
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
		return getStatistics(input);
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
		return getStatistics();
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
        executorService.scheduleAtFixedRate(dftAgent::monitorJobs, 10, 60, TimeUnit.SECONDS);
        logger.info("---------- Qunatum jobs are being monitored  ----------");
        System.out.println("---------- Qunatum jobs are being monitored  ----------");
       	
	}
	
	/**
	 * Monitors the currently running quantum jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * 
	 */
	private void monitorJobs() {
		scheduledIteration++;
		Workspace workspace = new Workspace();
		jobSpace = workspace.getWorkspaceName(Property.AGENT_WORKSPACE_DIR.getPropertyName(),
					Property.AGENT_CLASS.getPropertyName());
		try {
			if (session == null || scheduledIteration%10==0) {
				if(session!=null && session.isConnected()){
					session.disconnect();
				}
				System.out.println("Initialising a session.");
				session = jsch.getSession(username, server, 22);
				String pwd = getPassword(password);
				session.setPassword(pwd);
				session.setConfig("StrictHostKeyChecking", "no");
				session.connect();
				scheduledIteration = 0;
			}
			Map<String, List<String>> jobsRunning = new LinkedHashMap<>();
			Map<String, List<String>> jobsNotStarted = new LinkedHashMap<>();
			Utils.classifyJobs(jobsRunning, jobsNotStarted, jobSpace);
			System.out.println("Number of running jobs:" + jobsRunning.size());
			System.out.println("Number of not started jobs:" + jobsNotStarted.size());
			Map<String, List<String>> jobsRunningAfterUpdate = new LinkedHashMap<>();
			jobsRunningAfterUpdate.putAll(jobsRunning);
			updateRunningJobsStatus(jobsRunning, jobsRunningAfterUpdate);
			runNotStartedJobs(jobsNotStarted, jobsRunningAfterUpdate);
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
	 * Produces the statistics about quantum jobs.
	 * 
	 * @param input in json format containing the format in which the result</br> 
	 * should be codified, e.g. json. An example input file will look like</br>
	 * as follows:{"format":"json"}
	 * @return a string containing statistics in the user requested format.
	 * @throws IOException
	 */
	private String getStatistics(String input) throws IOException{
		JSONObject obj = new JSONObject(input);  
		String formatAccepts = obj.optString("format"); // json
		if(formatAccepts!=null && formatAccepts.toLowerCase().contains("json")){
			Workspace workspace = new Workspace();
			jobSpace = workspace.getWorkspaceName(Property.AGENT_WORKSPACE_DIR.getPropertyName(),
						Property.AGENT_CLASS.getPropertyName());
			JobStatistics jobStatistics = new JobStatistics(jobSpace);
			obj = new JSONObject();
			obj.put("Number of jobs currently running", jobStatistics.getJobsRunning());
			obj.put("Number of jobs successfully completed", jobStatistics.getJobsCompleted());
			obj.put("Number of jobs terminated with an error", jobStatistics.getJobsErrorTerminated());
			obj.put("Number of jobs to start", jobStatistics.getJobsNotStarted());
			obj.put("Total number of jobs submitted", jobStatistics.getJobsSubmitted());
			return obj.toString(); 
		}
		return new JSONObject().toString();
	}

	
	/**
	 * Produces the statistics about qunatum jobs.
	 * 
	 * @return
	 * @throws IOException
	 */
	private String getStatistics() throws IOException{
		Workspace workspace = new Workspace();
		jobSpace = workspace.getWorkspaceName(Property.AGENT_WORKSPACE_DIR.getPropertyName(),
					Property.AGENT_CLASS.getPropertyName());
		JobStatistics jobStatistics = new JobStatistics(jobSpace);
		String statistics = jobStatistics.getHTMLHeader();
		statistics = statistics + "<body>";
		statistics = statistics + "<center>";
		String headerText = "Statistics about jobs submitted to DFT Agent are shown in the table below:";
		statistics = statistics.concat(jobStatistics.getStatisticsTableHeader(headerText, "Property", "Value", "50%"));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs currently running", jobStatistics.getJobsRunning()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs successfully completed", jobStatistics.getJobsCompleted()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs terminated with an error", jobStatistics.getJobsErrorTerminated()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs to start", jobStatistics.getJobsNotStarted()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("<i>Total number of jobs submitted</i>", jobStatistics.getJobsSubmitted()+""));
		statistics = statistics + "</table>";
		statistics = statistics + "</center>";
		statistics = statistics + "</body>";
		statistics = statistics + "</html>";
		return statistics;
	}
	
	/**
	 * Updates the status of running jobs.
	 * 
	 * @param jobsRunning
	 * @param jobsRunningAfterUpdate
	 * @throws JSchException
	 * @throws SftpException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private void updateRunningJobsStatus(Map<String, List<String>> jobsRunning,
			Map<String, List<String>> jobsRunningAfterUpdate)
			throws JSchException, SftpException, IOException, InterruptedException {
		for (String runningJob : jobsRunning.keySet()) {
			File statusFile = Utils.getStatusFile(jobsRunning.get(runningJob));
			updateRunningJobsStatus(runningJob, statusFile, jobsRunningAfterUpdate);
		}
	}
	
	/**
	 * Starts running quantum jobs which were set up before. 
	 * 
	 * @param jobsNotStarted
	 * @param jobsRunning
	 * @throws SftpException
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 * @throws InterruptedException
	 */
	private void runNotStartedJobs(Map<String, List<String>> jobsNotStarted, Map<String, List<String>> jobsRunning)  throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		int runningJobsCount = jobsRunning.size();
		int howManyJobCanStart = Property.MAX_NUMBER_OF_JOBS.getValue() - runningJobsCount;
		int jobsStarted = 0;
		for(String jobNotStarted: jobsNotStarted.keySet()){
			if((howManyJobCanStart - jobsStarted) > 0){
				startJob(jobNotStarted, jobsNotStarted.get(jobNotStarted));
				jobsStarted++;
			}else{
				break;
			}
		}
	}
	
	/**
	 * Starts a quantum job.
	 * 
	 * @param job
	 * @param jobFiles
	 * @throws SftpException
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 * @throws InterruptedException
	 */
	private void startJob(String job, List<String> jobFiles) throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		// On HPC, the job folder and files have the address of machine where DFT Agent runs.
		// Therefore, the HPC address is replaced with the address of Agent machine.
		job = job.replace(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName(), Utils.getMachineAddress());
		String jobFolderOnHPC = createJobFolder(job);
		if(jobFolderOnHPC!=null){
			uploadFiles(jobFolderOnHPC, jobFiles);
		}
	}
		
	/**
	 * For running the current job, it uploads both the input file and</br>
	 * Slurm script to an HPC.
	 * 
	 * @param jobFolderOnHPC
	 * @param jobFiles
	 * @throws SftpException
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 * @throws InterruptedException
	 */
	private void uploadFiles(String jobFolderOnHPC, List<String> jobFiles) throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		String replacedInputFileName = "";
		String statusFileAbsolutePath = "";
		String jobId = "";
		for(String jobFile:jobFiles){
			if(jobFile.endsWith(Jobs.EXTENSION_SLURM_FILE.getName())){
				uploadFile(jobFile, jobFolderOnHPC);
			}
			if(jobFile.endsWith(Jobs.EXTENSION_INPUT_FILE.getName())){
				replacedInputFileName = getInputFileNameReplaced(jobFile);
				String inputFileNameOnHPC = jobFolderOnHPC.concat("/").concat(replacedInputFileName);
				uploadFile(jobFile, inputFileNameOnHPC);
				replaceFileContent(jobFolderOnHPC, inputFileNameOnHPC);
			}
			if(jobFile.endsWith(Jobs.STATUS_FILE.getName())){
				statusFileAbsolutePath = jobFile;
			}
		}
		if(!replacedInputFileName.isEmpty()){
			jobId = runQuantumJob(jobFolderOnHPC, replacedInputFileName);
		}
		if(!jobId.isEmpty()){
			Utils.addJobId(statusFileAbsolutePath, jobId);
		}
	}
	
	/**
	 * Produces the command to go to the current job directory and to run</br>
	 * the job.
	 * 
	 * @param jobFolderOnHPC
	 * @param inputFile
	 * @return
	 * @throws JSchException
	 * @throws IOException
	 */
	private String runQuantumJob(String jobFolderOnHPC, String inputFile) throws JSchException, IOException{
		inputFile = inputFile.replace(Jobs.EXTENSION_INPUT_FILE.getName(), "");
		String command = "cd ".concat(jobFolderOnHPC).concat(" && ")
				.concat("sbatch --job-name=").concat(inputFile).concat(" ")
				.concat(Property.SLURM_SCRIPT_FILE_NAME.getPropertyName());
		return runQuantumJob(command);
	}
	
	/**
	 * Modifies the name of the input file while copying this into the HPC cluster.
	 * 
	 * @param inputFile
	 * @return
	 * @throws UnknownHostException
	 */
	private String getInputFileNameReplaced(String inputFile) throws UnknownHostException{
		String tokens[];
		if(inputFile.contains("/")){
			tokens = inputFile.split("/");
		}else{
			tokens = inputFile.split("\\\\");
		}
		return tokens[tokens.length-1].replace(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName(), Utils.getMachineAddress());
	}
	
	/**
	 * Modifies the names of the check point file and log file provided in</br>
	 * the input file after copying this into the HPC cluster. 
	 * 
	 * @param jobFolderOnHPC
	 * @param replacedJobFileName
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 */
	private void replaceFileContent(String jobFolderOnHPC, String replacedJobFileName) throws JSchException, IOException, UnknownHostException{
		String command = "cd ".concat(jobFolderOnHPC).concat(" && ")
				.concat("sed -i 's/").concat(Property.HPC_CAMBRIDGE_ADDRESS.getPropertyName())
				.concat("/").concat(Utils.getMachineAddress()).concat("/g' ").concat(replacedJobFileName);
		executeCommand(command);
	}
	
	private String createJobFolder(String job) throws JSchException, IOException{
		// Creates the "mkdir" (make directory) command to create the workspace/jobspace directory.
		String command = "mkdir /home/".concat(username).concat("/").concat(jobSpace.getName());
		// Executes the command to create the workspace/jobspace directory.
		executeCommand(command);
		// Creates the command to create the job directory.
		command = "mkdir /home/".concat(username).concat("/").concat(jobSpace.getName()).concat("/").concat(job);
		// Executes the command for creating the job directory.
		executeCommand(command);
		return "/home/".concat(username).concat("/").concat(jobSpace.getName()).concat("/").concat(job);
	}
	
	/**
	 * Updates the latest status of the running jobs. 
	 * 
	 * @param runningJob
	 * @param statusFile
	 * @param jobsRunningAfterUpdate
	 * @throws JSchException
	 * @throws SftpException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private void updateRunningJobsStatus(String runningJob, File statusFile, Map<String, List<String>> jobsRunningAfterUpdate) throws JSchException, SftpException, IOException, InterruptedException{
		if(statusFile!=null){
			if(!isJobRunning(statusFile)){
				downloadFile(Utils.getLogFilePathOnHPC(runningJob, username, jobSpace), Utils.getJobLogFilePathOnAgentPC(runningJob, jobSpace));
				deleteJobOnHPC(Utils.getJobFolderPathOnHPC(runningJob, username, jobSpace));
				jobsRunningAfterUpdate.remove(runningJob);
				updateStatusForErrorTermination(statusFile, Utils.getJobLogFilePathOnAgentPC(runningJob, jobSpace));
			}
		}
	}
	
	/**
	 * Extracts from the log file if the current job terminated normally or</br>
	 * with error. In the case of error termination, updates the status of</br>
	 * the log file accordingly. 
	 * 
	 * @param statusFile
	 * @param jobFolderPathOnAgentPC
	 */
	private void updateStatusForErrorTermination(File statusFile, String jobFolderPathOnAgentPC) throws IOException{
		if(Utils.isErrorTermination(jobFolderPathOnAgentPC)){
			Utils.modifyStatus(statusFile.getAbsolutePath(), Jobs.JOB_LOG_MSG_ERROR_TERMINATION.getName());
		}
	}
	
	/**
	 * Checks if a job is running using the job id.
	 * 
	 * @param statusFile
	 * @return
	 * @throws JSchException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean isJobRunning(File statusFile) throws JSchException, IOException, InterruptedException{
		String jobId = Utils.getJobId(statusFile.getAbsolutePath());
		if(jobId==null){
			return false;
		} else{
			boolean isJobRunning = isJobRunning(jobId);
			if(isJobRunning){
				return true; 
			}else{
				Utils.modifyStatus(statusFile.getAbsolutePath(), Jobs.STATUS_JOB_COMPLETED.getName());
				return false;
			}
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
	 * @throws DFTAgentException
	 */
	public String setUpJob(String jsonString) throws IOException, DFTAgentException{
        	String message = setUpJobOnAgentMachine(jsonString);
			JSONObject obj = new JSONObject();
			obj.put("message", message);
        	return obj.toString();
    }
	
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, DFTAgentException{
		Workspace workspace = new Workspace();
		File workspaceFolder = workspace.createAgentWorkspace(Property.AGENT_WORKSPACE_DIR.getPropertyName(), Property.AGENT_CLASS.getPropertyName());
		if(workspaceFolder == null){
			return Jobs.JOB_SETUP_ERROR.getName();
		}else{
			return setUpQuantumJob(workspace, workspaceFolder, jsonString);
		}
	}
	
	private String setUpQuantumJob(Workspace ws, File workspaceFolder, String jsonString) throws IOException, DFTAgentException{
		OntoSpeciesKG oskg = new OntoSpeciesKG(); 
    	String speciesIRI = JSonRequestParser.getSpeciesIRI(jsonString);
    	if(speciesIRI == null && speciesIRI.trim().isEmpty()){
    		return Jobs.JOB_SETUP_SPECIES_IRI_MISSING.getName();
    	}
		String speciesGeometry = oskg.querySpeciesGeometry(speciesIRI);
		if(speciesGeometry == null && speciesGeometry.trim().isEmpty()){
    		return Jobs.JOB_SETUP_SPECIES_GEOMETRY_ERROR.getName();
    	}
		File jobFolder = ws.createJobFolder(workspaceFolder.getAbsolutePath());
    	if(createAllFileInJobFolder(ws, workspaceFolder, jobFolder, jsonString, speciesGeometry)==null){
    		return null;
    	}
    	return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}

	private String createAllFileInJobFolder(Workspace ws, File workspaceFolder, File jobFolder, String jsonString, String speciesGeometry) throws IOException, DFTAgentException{
		String inputFileMsg = ws.createInputFile(ws.getInputFilePath(jobFolder), jobFolder.getName(), speciesGeometry, jsonString);
		if(inputFileMsg == null){
			return Jobs.JOB_SETUP_INPUT_FILE_ERROR.getName();
		}
		String statusFileMsg = ws.createStatusFile(workspaceFolder, ws.getStatusFilePath(jobFolder));
		if(statusFileMsg == null){
			return null;
		}
		String jsonInputFileMsg = ws.createJSONInputFile(workspaceFolder, ws.getJSONInputFilePath(jobFolder), jsonString);
		if(jsonInputFileMsg == null){
			return null;
		}
		String scriptFileMsg = ws.copyScriptFile(getClass().getClassLoader().getResource(Property.SLURM_SCRIPT_FILE_NAME.getPropertyName()).getPath(), jobFolder.getAbsolutePath());
		if(scriptFileMsg == null){
			return null;
		}
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
	 * Checks if a job is still running using the job id.
	 * 
	 * @param jobId
	 * @return
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean isJobRunning(String jobId) throws JSchException, IOException, InterruptedException{
		String command = "squeue -j " + jobId + "--start";
		ArrayList<String> outputs = executeCommand(command);
		boolean jobRunning = isJobRunning(outputs);
		return jobRunning;
	}
	
	/**
	 * Analyses the outputs following the execution of the</br>
	 * job status check command to understand if the job</br>
	 * is still running or terminated.
	 * 
	 * @param outputs
	 * @return
	 */
	private boolean isJobRunning(ArrayList<String> outputs){
		if(outputs!=null && outputs.size()<=1){
			return false;
		}
		return true;
	}
	
	/**
	 * Extracts the job id from the 
	 * 
	 * @param outputs
	 * @return
	 */
	private String getJobId(ArrayList<String> outputs){
		for(String output: outputs){
			if(output.contains("Submitted batch job")){
				String tokens[] = output.split(" ");
				if(tokens.length>=4){
					return tokens[3].trim();
				}
			}
		}
		return null;
	}
	
	private String runQuantumJob(String command) throws JSchException, IOException{
		ArrayList<String> outputs = executeCommand(command);
		if (outputs == null) {
			return null;
		}
		String jobId = getJobId(outputs);
		System.out.println("Job id:" + jobId);
		return jobId;
	}
	
	/**
	 * Deletes the job folder including all files and folders belonging to it.
	 * 
	 * @param jobFolder the absolute path to the job folder.
	 * @throws JSchException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private void deleteJobOnHPC(String jobFolder) throws JSchException, IOException, InterruptedException{
		String command = "rm -r " + jobFolder;
		executeCommand(command);
	}
	
	/**
	 * Authenticate the user with the password.
	 * 
	 * @param username the name of user
	 * @param password the password of user
	 * @throws IOException
	 */
	public void authenticate(String username, String password) throws IOException{
        if(connection == null)
        	throw new IOException("Not connected to the server.");
        if (!connection.isAuthenticationComplete())
        	isAuthenticated = connection.authenticateWithPassword(username, password);
        if (isAuthenticated == false)
            throw new IOException("Authentication failed.");        		
	}
	
	/**
	 * Read the output of the most recently executed command.
	 * 
	 * @param br
	 * @return
	 * @throws IOException
	 */
	public ArrayList<String> readCommandOutput(BufferedReader br) throws IOException{
		if(br == null)
			throw new IOException("The reader is not initialised.");
		String line;
		ArrayList<String> outputs = new ArrayList<>();
		while((line=br.readLine())!=null){
			outputs.add(line);
		}
		br.close();
		return outputs;
	}
	
	/**
	 * Display every item in a list of string array.
	 * 
	 * @param list
	 */
	public void displayArray(ArrayList<String> list){
		for(String item:list){
			System.out.println(item);
		}
	}
	
	public void uploadFile(String src, String dest) throws JSchException, SftpException{
        System.out.println("Establishing a channel to transfer "+src+" to "+dest);
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();

		sftpChannel.put(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the channel.");
	}
	
	/**
	 * Downloads the log file.
	 * 
	 * @param src the file to be downloaded 
	 * @param dest the path under which the source file will be downloaded</br>
	 * 	or the absolute path containing the path and the file name.  
	 * @throws JSchException
	 * @throws SftpException
	 */
	public void downloadFile(String src, String dest) throws JSchException, SftpException{
        System.out.println("Establishing a channel to transfer "+src+" to "+dest);
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();
		sftpChannel.get(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the channel.");
	}

	/**
	 * Delete a folder or file name from an HPC, if the complete path is provided.</br>
	 * For example, to delete a folder called "test", user needs to provide "rds/.../test"
	 * 
	 * @param folderOrFileName 
	 * @throws JSchException
	 * @throws SftpException
	 */
	public void deleteFolderOrFile(String folderOrFileName) throws JSchException, SftpException, IOException{
		executeCommand("rm -r "+folderOrFileName);
	}
	
	public ArrayList<String> executeCommand(String Command) throws JSchException, IOException{
		ArrayList<String> outputs = null;
		System.out.println("Establishing a channel to perform the following command:" + Command);
		Channel execChannel = session.openChannel("exec");
		((ChannelExec) execChannel).setCommand(Command);
		execChannel.setInputStream(null);
		((ChannelExec) execChannel).setErrStream(System.err);
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(execChannel.getInputStream()));
		execChannel.connect();
		outputs = readCommandOutput(stdInput);
		execChannel.disconnect();
		System.out.println("Closing the channel.");
		return outputs;
	}
	
	/**
	 * Decodes the password.
	 * 
	 * @param password encrypted password.
	 * @return
	 */
	private String getPassword(String password){
		return password.replace("l", "1").replace("_", "").replace("7", "3").replace("3", "4");
	}
}
