package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import org.slf4j.LoggerFactory;
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
import ch.ethz.ssh2.StreamGobbler;
import ch.ethz.ssh2.Session;

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
	String command = "ls";
	Connection connection;
	Session session;
	boolean isAuthenticated;
	static HashSet<String> jobPool = new HashSet<>();
	
	private static File taskSpace; 
	
	public static void main(String[] args) throws ServletException, DFTAgentException{
		DFTAgent dftAgent = new DFTAgent();
		dftAgent.monitorJobs();
//		dftAgent.init();
	}
	
	/**
     * Allows to perform a SPARQL query of any complexity.</br>
     * It returns the results in JSON format.
     * 
     * @param outputFormat the output format, e.g. JSON or CSV
     * @param kbiri The IRI of the Knowledge Base on which user is interested 
     * to perform a query. 
     * @param query The current query user is willing to perform.
     * @return
     */
	@RequestMapping(value="/job/request", method = RequestMethod.GET)
    @ResponseBody
    public String query(@RequestParam String input) throws IOException, DFTAgentException{
		System.out.println("received query:\n"+input);
		logger.info("received query:\n"+input);
		return setUpJob(input);
    }
	
	/**
	 * Starts the scheduler to monitor the tasks.
	 * 
	 * @throws DFTAgentException
	 */
	public void init() throws ServletException{
        logger.info("---------- Quantum Calculation Agent has started ----------");
        System.out.println("---------- Quantum Calculation Agent has started ----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        DFTAgent dftAgent = new DFTAgent();
       	executorService.scheduleAtFixedRate(dftAgent::monitorJobs, 60, 60, TimeUnit.SECONDS);
        logger.info("---------- Qunatum jobs are being monitored  ----------");
        System.out.println("---------- Qunatum jobs are being monitored  ----------");
       	
	}

	private void monitorJobs() {
		Workspace workspace = new Workspace();
		if (taskSpace == null) {
			taskSpace = workspace.getWorkspaceName(Property.AGENT_WORKSPACE_DIR.getPropertyName(),
					Property.AGENT_CLASS.getPropertyName());
		}
		try {
			Map<String, List<String>> jobsRunning = new LinkedHashMap<>();
			Map<String, List<String>> jobsNotStarted = new LinkedHashMap<>();
			Utils.classifyJobs(jobsRunning, jobsNotStarted, taskSpace);
			System.out.println("Number of unfinished jobs:" + jobsRunning.size());
			System.out.println("Number of not started jobs:" + jobsNotStarted.size());
			updateRunningJobsStatus(jobsRunning);
			runNotStartedJobs(jobsNotStarted, jobsRunning);
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

	private void updateRunningJobsStatus(Map<String, List<String>> jobsRunning) throws IOException, InterruptedException{
		for(String runningJob:jobsRunning.keySet()){
			File statusFile = Utils.getStatusFile(jobsRunning.get(runningJob));
			updateRunningJobsStatus(jobsRunning, runningJob, statusFile);
		}
	}
	
	private void runNotStartedJobs(Map<String, List<String>> jobsNotStarted, Map<String, List<String>> jobsRunning)  throws SftpException, JSchException{
		int runningJobsCount = jobsRunning.size();
		int howManyJobCanStart = Property.MAX_NUMBER_OF_JOBS.getValue() - runningJobsCount;
		int jobsStarted = 0;
		for(String jobNotStarted: jobsNotStarted.keySet()){
			if((howManyJobCanStart - jobsStarted) > 0){
				startJob(jobNotStarted, jobsNotStarted.get(jobNotStarted));
				jobsStarted++;
			}
		}
	}
	
	private void startJob(String job, List<String> jobFiles) throws SftpException, JSchException{
		String jobFolderOnHPC = createJobFolder(job);
		if(jobFolderOnHPC!=null){
			uploadFiles(jobFolderOnHPC, jobFiles);
		}
	}
	
	private void uploadFiles(String jobFolderOnHPC, List<String> jobFiles) throws SftpException, JSchException{
		for(String jobFile:jobFiles){
			if(jobFile.endsWith(Jobs.EXTENSION_SLURM_FILE.getName()) || jobFile.endsWith(Jobs.EXTENSION_INPUT_FILE.getName())){
				uploadFile(jobFile, jobFolderOnHPC);
			}
		}
	}
	
	private boolean isJobFolderCreated(String job){
		String command = "cd /home/".concat(username).concat("/").concat(taskSpace.getName()).concat("/").concat(job);
		List<String> outputs = executeCommand(command);
		if(outputs.size()<=0){
			return true;
		}
		return false;
	}
	
	private String createJobFolder(String job){
		// Creates the "mkdir" (make directory) command to create the workspace/jobspace directory.
		command = "mkdir /home/".concat(username).concat("/").concat(taskSpace.getName());
		// Executes the command to create the workspace/jobspace directory.
		executeCommand(command);
		// Creates the command to create the job directory.
		command = "mkdir /home/".concat(username).concat("/").concat(taskSpace.getName()).concat("/").concat(job);
		// Executes the command for creating the job directory.
		List<String> outputs = executeCommand(command);
		if(outputs.size()>0){
			return null;
		}
		return "/home/".concat(username).concat("/").concat(taskSpace.getName()).concat("/").concat(job);
	}
	
	
	
	private void updateRunningJobsStatus(Map<String, List<String>> jobsRunning, String runningJob, File statusFile) throws IOException, InterruptedException{
		if(statusFile!=null){
			if(!isJobRunning(statusFile)){
				jobsRunning.remove(runningJob);
			}
		}
	}
	
	private boolean isJobRunning(File statusFile) throws IOException, InterruptedException{
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
	
	private String setUpJob(String jsonString) throws IOException, DFTAgentException{
        	return setUpJobOnAgentMachine(jsonString);
    }
	
	private String setUpJobOnAgentMachine(String jsonString) throws IOException, DFTAgentException{
		Workspace workspace = new Workspace();
		File workspaceFolder = workspace.createAgentWorkspace(Property.AGENT_WORKSPACE_DIR.getPropertyName(), Property.AGENT_CLASS.getPropertyName());
		if(workspaceFolder == null){
			return Jobs.JOB_SETUP_ERROR.getName();
		}else{
			File jobFolder = workspace.createJobFolder(workspaceFolder.getAbsolutePath());
			if(jobFolder == null){
				return Jobs.JOB_SETUP_ERROR.getName();
			}else{
				return setUpQuantumJob(workspace, workspaceFolder, jobFolder, jsonString);
			}
		}
	}
	
	private String setUpQuantumJob(Workspace ws, File workspaceFolder, File jobFolder, String jsonString) throws IOException, DFTAgentException{
		OntoSpeciesKG oskg = new OntoSpeciesKG(); 
    	String speciesIRI = JSonRequestParser.getSpeciesIRI(jsonString);
    	if(speciesIRI == null && speciesIRI.trim().isEmpty()){
    		return Jobs.JOB_SETUP_SPECIES_IRI_MISSING.getName();
    	}
		String speciesGeometry = oskg.querySpeciesGeometry(speciesIRI);
    	if(speciesGeometry == null && speciesGeometry.trim().isEmpty()){
    		return Jobs.JOB_SETUP_SPECIES_GEOMETRY_ERROR.getName();
    	}
		System.out.println("SpeciesGeometry:"+speciesGeometry);
		String inputFileMsg = ws.createInputFile(ws.getInputFilePath(jobFolder), jobFolder.getName(), speciesGeometry, jsonString);
		if(inputFileMsg == null){
			return Jobs.JOB_SETUP_INPUT_FILE_ERROR.getName();
		}
		String statusFileMsg = ws.createStatusFile(workspaceFolder, ws.getStatusFilePath(jobFolder));
		if(statusFileMsg == null){
			return null;
		}
		String scriptFileMsg = ws.copyScriptFile(Property.SLURM_SCRIPT_FILE_PATH.getPropertyName(), jobFolder.getAbsolutePath());
		if(scriptFileMsg == null){
			return null;
		}
		return Jobs.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
	 * Runs a set of quantum jobs.
	 * 
	 * @return
	 */
	private String runQuantumJob(){
		try {
			setupJob(); // In the next iteration of development, we will include code for setting up jobs.   
			String slurmScriptName = "G09Slurm_darwin_S1.sh";
			String inputFileName = "water_ex.com";
//			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(inputFileName),
//					"rds/hpc-work/gaussian");
//			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(slurmScriptName),
//					"rds/hpc-work/gaussian");
			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(inputFileName),
			"/home/".concat(username));
			uploadFile("C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras/".concat(slurmScriptName),
			"/home/".concat(username));
			
			runQuantumJob(slurmScriptName, inputFileName);
		} catch (JSchException e) {
			logger.error(e.getMessage());
		} catch (SftpException e) {
			logger.error(e.getMessage());
		} catch (InterruptedException e) {
			logger.error(e.getMessage());
		} catch (IOException e) {
			logger.error(e.getMessage());
		}
		return null;
	}
	
	/**
	 * Decides the time interval between the current and next</br>
	 * status check operations of a quantum job.   
	 * 
	 * @param count
	 * @throws InterruptedException
	 */
	private void waitBeforeStatusCheck() throws InterruptedException{
		Thread.sleep(2000);
	}
	
	/**
	 * Checks if a job is still running using the job id.
	 * 
	 * @param jobId
	 * @return
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean isJobRunning(String jobId) throws IOException, InterruptedException{
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
		if(outputs!=null && outputs.size()==1){
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
	
	private void setupJob(){
		
	}
	
	/**
	 * Runs a quantum job and copies the output file (log file) from CSD3
	 * to the machine where DFT Agent is hosted.
	 * 
	 * @param command
	 * @throws IOException
	 */
	private String runQuantumJob(String scriptName, String inputFileName) throws IOException, InterruptedException{
//		String command = "cd rds/hpc-work/gaussian && sbatch ".concat(scriptName);
		String command = "cd /home/".concat(username).concat(" && sbatch ").concat(scriptName);

		ArrayList<String> outputs = executeCommand(command);
		if (outputs == null) {
			return null;
		}
		String jobId = getJobId(outputs);
		System.out.println("Job id:" + jobId);
		jobPool.add(jobId);
		boolean isJobRunning = isJobRunning(jobId);
		int count = 0;
		System.out.println("Is the job running? " + isJobRunning);
		while (isJobRunning) {
			count++;
			waitBeforeStatusCheck();
			isJobRunning = isJobRunning(jobId);
		}
		try {
//			downloadFile("rds/hpc-work/gaussian/".concat(inputFileName.replace(".com", ".log")),
//					"C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras");
			downloadFile("/home/msff2/".concat(inputFileName.replace(".com", ".log")),
			"C:/Users/msff2/Documents/HPC/KnowledgeCapturedFromAngiras");
		} catch (JSchException e) {
			logger.error(e.getMessage());
		} catch (SftpException e) {
			logger.error(e.getMessage());
		}
		return jobId;
	}
	
	/**
	 * Connects the client to the server via server IP address or name. 
	 * 
	 * @param server IP address or name
	 * @throws IOException
	 */
	public void connect(String server) throws IOException{
        if(connection == null){
        	connection = new Connection(server);
            connection.connect();
    		authenticate(username, getPassword(password));
    		openSession();
        }
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
	 * Open a session in order to allow to execute commands.
	 * 
	 * @throws IOException
	 */
	public void openSession() throws IOException{
		if(connection == null)
        	throw new IOException("Not connected to the server.");
		if (!connection.isAuthenticationComplete())
			throw new IOException("Authentication is not complete.");
		session = connection.openSession();
	}
	
	/**
	 * Executes a command within a session.
	 * 
	 * @param command
	 * @throws IOException
	 */
	public void executeCommand1(String command) throws IOException{
		if(session == null)
			throw new IOException("No session is open.");
		session.execCommand(command);
	}
	
	/**
	 * Create the reader to read the outputs of the most recently executed</br>
	 * command.
	 * 
	 * @return
	 * @throws IOException
	 */
	public BufferedReader getReader() throws IOException{
        if(session == null)
        	throw new IOException("No session is open.");
        return new BufferedReader(new InputStreamReader(new StreamGobbler(session.getStdout())));
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
		return outputs;
	}
	
	/**
	 * Close the currently open session and connection. 
	 * 
	 * @throws IOException
	 */
	public void closeSessionAndConnection() throws IOException{
        if(session == null)
        	throw new IOException("No session is open.");
		session.close();
        if(connection == null)
        	throw new IOException("No connection is open.");
		connection.close();
		System.out.println("Connection closed test:"+(session==null)+" session is null:"+session.getState());
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
		JSch jsch = new JSch();
		com.jcraft.jsch.Session session = jsch.getSession(username, server);
		String pwd = getPassword(password);
		session.setPassword(pwd);
        session.setConfig("StrictHostKeyChecking", "no");
        System.out.println("Establishing Connection to transfer "+src+" to "+dest);
        session.connect();
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();

		sftpChannel.put(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the connection.");
	}
	
	public void downloadFile(String src, String dest) throws JSchException, SftpException{
		JSch jsch = new JSch();
		com.jcraft.jsch.Session session = jsch.getSession(username, server);
		String pwd = getPassword(password);
		session.setPassword(pwd);
        session.setConfig("StrictHostKeyChecking", "no");
        System.out.println("Establishing Connection to transfer "+src+" to "+dest);
        session.connect();
		ChannelSftp sftpChannel = (ChannelSftp) session.openChannel("sftp");
		sftpChannel.connect();

		sftpChannel.get(src, dest);
		sftpChannel.disconnect();
		System.out.println("Closing the connection.");
	}

	/**
	 * Delete a folder or file name from an HPC, if the complete path is provided.</br>
	 * For example, to delete a folder called "test", user needs to provide "rds/.../test"
	 * 
	 * @param folderOrFileName 
	 * @throws JSchException
	 * @throws SftpException
	 */
	public void deleteFolderOrFile(String folderOrFileName) throws JSchException, SftpException{
		executeCommand("rm -r "+folderOrFileName);
	}

	/**
	 * Create a folder on an HPC, if the complete path is provided.</br>
	 * For example, to create a folder called "test", user needs to provide "rds/.../test"
	 * 
	 * @param folder 
	 * @throws JSchException
	 * @throws SftpException
	 */
	public void createFolder(String folder) throws JSchException, SftpException{
		executeCommand("mkdir "+folder);
	}
	
	public ArrayList<String> executeCommand(String Command){
		ArrayList<String> outputs = null;
		try {
			JSch jsch = new JSch();
			com.jcraft.jsch.Session session = jsch.getSession(username, server);
			String pwd = getPassword(password);
			session.setPassword(pwd);
			session.setConfig("StrictHostKeyChecking", "no");
			System.out.println("Establishing a connection to perform the following command:" + Command);
			session.connect();
			Channel channel = session.openChannel("exec");
			((ChannelExec) channel).setCommand(Command);
			channel.setInputStream(null);
			((ChannelExec) channel).setErrStream(System.err);
			BufferedReader stdInput = new BufferedReader(new InputStreamReader(channel.getInputStream()));
			channel.connect();
			outputs = readCommandOutput(stdInput);
			channel.disconnect();
			session.disconnect();
			System.out.println("DONE");
		} catch (JSchException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return outputs;
	}

	public void SSHClient(String serverIp,String command, String username,String password) throws IOException{
        System.out.println("inside the ssh function");
        try
        {
            Connection conn = new Connection(serverIp);
            conn.connect();
            boolean isAuthenticated = conn.authenticateWithPassword(username, password);
            if (isAuthenticated == false)
                throw new IOException("Authentication failed.");        
            ch.ethz.ssh2.Session sess = conn.openSession();
            sess.execCommand(command);  
            InputStream stdout = new StreamGobbler(sess.getStdout());
            BufferedReader br = new BufferedReader(new InputStreamReader(stdout));
            System.out.println("the output of the command is");
            while (true)
            {
                String line = br.readLine();
                if (line == null)
                    break;
                System.out.println(line);
            }
            System.out.println("ExitCode: " + sess.getExitStatus());
            sess.close();
            conn.close();
        }
        catch (IOException e)
        {
            e.printStackTrace(System.err);

        }
    }
	
	private String getPassword(String password){
		return password.replace("l", "1").replace("_", "").replace("7", "3").replace("3", "4");
	}
}
