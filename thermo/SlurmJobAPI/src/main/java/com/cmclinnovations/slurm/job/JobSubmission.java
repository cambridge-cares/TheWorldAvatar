package com.cmclinnovations.slurm.job;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;

/**
 * The API developed for setting-up and running jobs Slurm jobs.
 * 
 * @author msff2
 *
 */
public class JobSubmission{
	private Logger logger = LoggerFactory.getLogger(JobSubmission.class);	
	private String hpcAddress;
	private String username = "msff2";
	private String password = getPassword("Abcdl955_l7_l7_l7_aB");
	private int delayBeforeStart = 10;
	private int interval = 60;
	private String agentClass;
	private File agentWorkspaceDirectory;
	boolean isAuthenticated;
	private File jobSpace;
	
	static Session session;
	static JSch jsch = new JSch();
	
	static int scheduledIteration = 0;
	static List<String> jobsRunning = new ArrayList<String>();
	
	public String getHpcAddress() {
		return hpcAddress;
	}

	public void setHpcAddress(String hpcAddress) {
		this.hpcAddress = hpcAddress;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}
	
	public String getAgentClass() {
		return agentClass;
	}
	
	public void setAgentClass(String agentClass) {
		this.agentClass = agentClass;
	}
	
	public File getAgentWorkspaceDirectory() {
		return agentWorkspaceDirectory;
	}
	
	public void setAgentWorkspaceDirectory(File agentWorkspaceDirectory) {
		this.agentWorkspaceDirectory = agentWorkspaceDirectory;
	}
	
	public static void main(String[] args) throws SlurmJobException{
		JobSubmission jobSubmission = new JobSubmission("DFTAgent", new File(System.getProperty("user.home")), "login-skylake.hpc.cam.ac.uk");
		jobSubmission.init();
	}

	public int getDelayBeforeStart() {
		return delayBeforeStart;
	}

	public void setDelayBeforeStart(int delayBeforeStart) {
		this.delayBeforeStart = delayBeforeStart;
	}

	public int getInterval() {
		return interval;
	}

	public void setInterval(int interval) {
		this.interval = interval;
	}
	
	/**
	 * Constructor for this class.
	 * 
	 * @param agentClass the name of the agent class (e.g. DFTAgent and EBRAgent).
	 * @param agentWorkspaceDirectory the directory where the workspace<br>
	 * will be created, e.g. user home directory read by System.getProperty("user.home");
	 * @param hpcAddress the address of HPC, e.g. login-skylake.hpc.cam.ac.uk for CSD3.
	 */
	public JobSubmission(String agentClass, File agentWorkspaceDirectory, String hpcAddress){
		this.agentClass = agentClass;
		this.agentWorkspaceDirectory = agentWorkspaceDirectory;
		this.hpcAddress = hpcAddress;
		init();
	}
	
    /**
     * Allows to submit a job request and it returns success or failure<br> 
     * message in JSON format.</br>
     * 
     * @param jsonInput the request from user in JSON format to perform a<br> 
     * Slurm job.
     * @param slurmScript the Slurm script developed for a specific HPC facility.
     * @param input the input.zip file containing all necessary inputs including<br>
     * folders, subfolders and files.
     * 
     * @return
     * @throws IOException
     * @throws SlurmJobException
     */
	public String jobRequest(String jsonInput, File slurmScript, File input) throws IOException, SlurmJobException{
		System.out.println("received query:\n"+input);
		logger.info("received query:\n"+input);
		return setUpJob(jsonInput, slurmScript, input);
    }
	
	/**
	 * Sets up a quantum job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br.
	 * - the Status file.</br>
	 * - the JSON input file, which comes from the user request.</br>
	 * 
	 * @param jsonInput
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	public String setUpJob(String jsonInput, File slurmScript, File input) throws IOException, SlurmJobException{
        	String message = setUpJobOnAgentMachine(jsonInput, slurmScript, input);
			JSONObject obj = new JSONObject();
			obj.put("message", message);
        	return obj.toString();
    }
	
	/**
	 * Sets up the quantum job for the current input.
	 *   
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonString, File slurmScript, File input) throws IOException, SlurmJobException{
		Workspace workspace = new Workspace();
		File workspaceFolder = workspace.getWorkspaceName(getAgentWorkspaceDirectory().getAbsolutePath(), getAgentClass());
		if(workspaceFolder == null){
			return Status.JOB_SETUP_ERROR.getName();
		}else{
			return setUpQuantumJob(workspace, workspaceFolder, jsonString, slurmScript, input);
		}
	}
	
	/**
	 * Sets up the quantum job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonString
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpQuantumJob(Workspace ws, File workspaceFolder, String jsonString, File slurmScript, File input) throws IOException, SlurmJobException{
		File jobFolder = ws.createJobFolder(workspaceFolder.getAbsolutePath(), getHpcAddress());
    	if(createAllFileInJobFolder(ws, workspaceFolder, jobFolder, jsonString, input)==null){
    		return null;
    	}
    	return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
	 * Creates all files relevant for the current job, in particular, it</br>
	 * creates the following files:</br>
	 * - the input file in com (.com) format for running the job on an HPC
	 * - the status file in text (.txt) format
	 * - the input file in json (.json) format
	 * - the Slurm script file in shell script (.sh) format 
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jobFolder
	 * @param jsonString
	 * @param speciesGeometry
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String createAllFileInJobFolder(Workspace ws, File workspaceFolder, File jobFolder, String jsonString, File input) throws IOException, SlurmJobException{
		String inputFileMsg = ws.createInputFile(ws.getInputFilePath(jobFolder, getHpcAddress(), ws.getInputFileExtension(input)), input);
		if(inputFileMsg == null){
			return Status.JOB_SETUP_INPUT_FILE_ERROR.getName();
		}
		String statusFileMsg = ws.createStatusFile(workspaceFolder, ws.getStatusFilePath(jobFolder), getHpcAddress());
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
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
     * Shows the following statistics of Slurm jobs.</br>
     * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @param input the JSON string specifying the return data format, e.g. JSON.
     * @return the statistics in JSON format if requested. 
     */
    public String produceStatistics(String input) throws IOException, SlurmJobException{
		System.out.println("Received a request to send statistics.\n");
		logger.info("Received a request to send statistics.\n");
		return "";
    }
	
	/**
     * Shows the following statistics of Slurm jobs.</br>
     * - Total number of jobs submitted
     * - Total number of jobs currently running  
     * - Total number of jobs successfully completed
     * - Total number of jobs terminated with an error
     * - Total number of jobs not started yet
     * 
     * @return the statistics in HTML format. 
     */
    public String showStatistics() throws IOException, SlurmJobException{
		System.out.println("Received a request to show statistics.\n");
		logger.info("Received a request to show statistics.\n");
		return "";
    }
	
	/**
	 * Starts the scheduler to monitor Slurm jobs.
	 * 
	 * @throws SlurmJobException
	 */
	public void init(){
        logger.info("----------Slurm Job Submission Component has started----------");
        System.out.println("----------Slurm Job Submission Component has started----------");
        ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
        JobSubmission jobs = new JobSubmission(getAgentClass(), getAgentWorkspaceDirectory(), getHpcAddress());
       	// 10 refers to the delay (in seconds) before the job scheduler
        // starts and 60 refers to the interval between two consecutive
        // executions of the scheduler.
        executorService.scheduleAtFixedRate(jobs::monitorJobs, getDelayBeforeStart(), getInterval(), TimeUnit.SECONDS);
        logger.info("----------Slurm Jobs are being monitored  ----------");
        System.out.println("---------- Slurm Jobs are being monitored  ----------");
       	
	}
	
	/**
	 * Monitors the currently running jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * 
	 */
	private void monitorJobs() {
		scheduledIteration++;
		Workspace workspace = new Workspace();
	}
	
	/**
	 * Checks the status of job on the HPC where it was submitted.
	 * 
	 * @param jobId
	 * @param statusFile 
	 * @return
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean isJobRunning(String jobId, File statusFile) throws JSchException, IOException, InterruptedException{
		String command = "squeue -j " + jobId;
		ArrayList<String> outputs = executeCommand(command);
		boolean jobRunning = isJobRunning(outputs, statusFile);
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
	private boolean isJobRunning(ArrayList<String> outputs, File statusFile) throws IOException{
		if(outputs!=null && outputs.size()<=1){
			return false;
		}
		if(outputs!=null){
			String[] headers = outputs.get(0).split("\\s+");
			String[] values = outputs.get(1).split("\\s+");
			for(int i=0; i<headers.length; i++){
				if(headers[i].toLowerCase().equals("st")){
					if(values.length>i){
//						updateStatus(values[i], statusFile);
					}
				}
			}
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
	
	/**
	 * Runs a Slurm job.
	 * 
	 * @param command
	 * @return
	 * @throws JSchException
	 * @throws IOException
	 */
	private String runSlurmJob(String command) throws JSchException, IOException{
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
	 * Reads the output of the most recently executed command.
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
	 * Displays every item in a list of string array.
	 * 
	 * @param list
	 */
	public void displayArray(ArrayList<String> list){
		for(String item:list){
			System.out.println(item);
		}
	}
	
	/**
	 * Uploads a file from the source folder belonging to any machine to</br> 
	 * the destination folder on the same machine or any other machine.
	 *   
	 * @param src
	 * @param dest
	 * @throws JSchException
	 * @throws SftpException
	 */
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
	 * Deletes a folder or file name from an HPC, if the complete path is provided.</br>
	 * For example, to delete a folder called "test", user needs to provide "rds/.../test"
	 * 
	 * @param folderOrFileName 
	 * @throws JSchException
	 * @throws SftpException
	 */
	public void deleteFolderOrFile(String folderOrFileName) throws JSchException, SftpException, IOException{
		executeCommand("rm -r "+folderOrFileName);
	}
	
	/**
	 * Executes any command that can be run on the Linux console.
	 * 
	 * @param Command
	 * @return
	 * @throws JSchException
	 * @throws IOException
	 */
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
