package uk.ac.cam.cares.jps.base.slurm.job;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelExec;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.IdentityRepository;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;
import com.jcraft.jsch.agentproxy.AgentProxyException;
import com.jcraft.jsch.agentproxy.Connector;
import com.jcraft.jsch.agentproxy.RemoteIdentityRepository;
import com.jcraft.jsch.agentproxy.connector.PageantConnector;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

/**
 * This is an API developed for setting-up and running Slurm jobs.
 * 
 * @author msff2
 *
 */
public class JobSubmission{
	private Logger logger = LoggerFactory.getLogger(JobSubmission.class);	
	private String hpcAddress;
	private String agentClass;
	private File workspaceDirectory;
	private String workspaceName;
	private String workspaceParentPath;
	boolean isAuthenticated;
	
	public SlurmJobProperty slurmJobProperty = new SlurmJobProperty();
	
	static Session session;
	static JSch jsch = new JSch();
	
	static int scheduledIteration = 0;
	static Set<String> jobsRunning = new HashSet<String>();
	
	public String getHpcAddress() {
		return hpcAddress;
	}

	public void setHpcAddress(String hpcAddress) {
		this.hpcAddress = hpcAddress;
	}
	
	public String getAgentClass() {
		return agentClass;
	}
	
	public void setAgentClass(String agentClass) {
		this.agentClass = agentClass;
	}
	
	public File getWorkspaceDirectory() {
		return workspaceDirectory;
	}

	public void setWorkspaceDirectory(File workspaceDirectory) {
		this.workspaceDirectory = workspaceDirectory;
	}

	public String getWorkspaceName() {
		return workspaceName;
	}

	public void setWorkspaceName(String workspaceName) {
		this.workspaceName = workspaceName;
	}

	public static void main(String[] args) throws SlurmJobException{
//		JobSubmission jobSubmission = new JobSubmission("DFTAgent", new File(System.getProperty("user.home")), "login-skylake.hpc.cam.ac.uk");
//		jobSubmission.init();
	}
	
	public String getWorkspaceParentPath() {
		return workspaceParentPath;
	}

	public void setWorkspaceParentPath(String workspaceParentPath) {
		this.workspaceParentPath = workspaceParentPath;
	}

	/**
	 * Constructor for this class.
	 * 
	 * @param agentClass the name of the agent class (e.g. DFTAgent and EBRAgent).
	 * @param workspaceName the name of workspace<br>
	 * will be created, e.g. user home directory read by System.getProperty("user.home");
	 * @param hpcAddress the address of HPC, e.g. login-skylake.hpc.cam.ac.uk for CSD3.
	 */
	public JobSubmission(String agentClass, String hpcAddress){
		this.agentClass = agentClass;
		this.hpcAddress = hpcAddress;
		this.workspaceDirectory = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), agentClass);
		this.workspaceParentPath = Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName();
	}
	
	/**
	 * Sets up a Slurm job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br.
	 * - the Status file.</br>
	 * - the JSON input file, which comes from the user request.</br>
	 * 
	 * @param jsonInput
	 * @param slurmScript
	 * @param input
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	public String setUpJob(String jsonInput, File slurmScript, File input, long timeStamp) throws IOException, SlurmJobException{
        	String message = setUpJobOnAgentMachine(jsonInput, slurmScript, input, timeStamp);
        	return message;
    }

	/**
	 * Sets up a Slurm job by creating the job folder and the following files</br>
	 * under this folder:</br>
	 * - the input file.</br>
	 * - the Slurm script file.</br.
	 * - the Status file.</br>
	 * - the JSON input file, which comes from the user request.</br>
	 * 
	 * @param jsonInput
	 * @param slurmScript
	 * @param input
	 * @param executable
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	public String setUpJob(String jsonInput, File slurmScript, File input, File executable, long timeStamp) throws IOException, SlurmJobException{
        	String message = setUpJobOnAgentMachine(jsonInput, slurmScript, input, executable, timeStamp);
			JSONObject obj = new JSONObject();
			obj.put("message", message);
        	return obj.toString();
    }

	
	/**
	 * Sets up the Slurm job for the current input.
	 *   
	 * @param jsonString
	 * @param slurmScript
	 * @param input
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonString, File slurmScript, File input, long timeStamp) throws IOException, SlurmJobException{
		Workspace workspace = new Workspace();
		if(workspaceDirectory == null){
			return Status.JOB_SETUP_ERROR.getName();
		}else{
			return setUpSlurmJob(workspace, workspaceDirectory, jsonString, slurmScript, input, timeStamp);
		}
	}
	
	/**
	 * Sets up the Slurm job for the current input.
	 *   
	 * @param jsonString
	 * @param slurmScript
	 * @param input
	 * @param executable
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpJobOnAgentMachine(String jsonString, File slurmScript, File input, File executable, long timeStamp) throws IOException, SlurmJobException{
		Workspace workspace = new Workspace();
		if(workspaceDirectory == null){
			return Status.JOB_SETUP_ERROR.getName();
		}else{
			return setUpSlurmJob(workspace, workspaceDirectory, jsonString, slurmScript, input, executable, timeStamp);
		}
	}
	
	/**
	 * Sets up the Slurm job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonString
	 * @param slurmScript
	 * @param input
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpSlurmJob(Workspace ws, File workspaceFolder, String jsonString, File slurmScript, File input, long timeStamp) throws IOException, SlurmJobException{
		File jobFolder = ws.createJobFolder(workspaceFolder.getAbsolutePath(), getHpcAddress(), timeStamp);
    	if(createAllFileInJobFolder(ws, workspaceFolder, jobFolder, jsonString, slurmScript, input)==null){
    		return null;
    	}
    	return jobFolder.getName();
	}
	
	/**
	 * Sets up the Slurm job for the current request.
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jsonString
	 * @param slurmScript
	 * @param input
	 * @param executable
	 * @param timeStamp
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String setUpSlurmJob(Workspace ws, File workspaceFolder, String jsonString, File slurmScript, File input, File executable, long timeStamp) throws IOException, SlurmJobException{
		File jobFolder = ws.createJobFolder(workspaceFolder.getAbsolutePath(), getHpcAddress(), timeStamp);
    	if(createAllFileInJobFolder(ws, workspaceFolder, jobFolder, jsonString, slurmScript, input, executable)==null){
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
	 * @param slurmScript
	 * @param input
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String createAllFileInJobFolder(Workspace ws, File workspaceFolder, File jobFolder, String jsonString, File slurmScript, File input) throws IOException, SlurmJobException{
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
		String scriptFileMsg = ws.copyFile(slurmScript.getAbsolutePath(), jobFolder.getAbsolutePath().concat(File.separator).concat(slurmScript.getName()));
		if(scriptFileMsg == null){
			return null;
		}
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
	 * Creates all files relevant for the current job, in particular, it</br>
	 * creates the following files:</br>
	 * - the input file in zip (.zip) format for running the job on an HPC
	 * - the status file in text (.txt) format
	 * - the input file in json (.json) format
	 * - the Slurm script file in shell script (.sh) format
	 * - the executable file in java is in jar (.jar) format
	 * 
	 * @param ws
	 * @param workspaceFolder
	 * @param jobFolder
	 * @param jsonString
	 * @param slurmScript
	 * @param input
	 * @param executable
	 * @return
	 * @throws IOException
	 * @throws DFTAgentException
	 */
	private String createAllFileInJobFolder(Workspace ws, File workspaceFolder, File jobFolder, String jsonString, File slurmScript, File input, File executable) throws IOException, SlurmJobException{
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
		String scriptFileMsg = ws.copyFile(slurmScript.getAbsolutePath(), jobFolder.getAbsolutePath().concat(File.separator).concat(slurmScript.getName()));
		if(scriptFileMsg == null){
			return null;
		}
		String executableFileMsg = ws.copyFile(executable.getAbsolutePath(), jobFolder.getAbsolutePath().concat(File.separator).concat(executable.getName()));
		if(executableFileMsg == null){
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
	 * Produces the statistics about Slurm jobs.
	 * 
	 * @param input in json format containing the format in which the result</br> 
	 * should be codified, e.g. json. An example input file will look like</br>
	 * as follows:{"format":"json"}
	 * @return a JSONObject containing statistics in the user requested format.
	 * @throws IOException
	 */
	public JSONObject getStatistics(String input) throws IOException{
		JSONObject obj = new JSONObject(input);  
		String formatAccepts = obj.optString("format"); // json
		if(formatAccepts!=null && formatAccepts.toLowerCase().contains("json")){
			JobStatistics jobStatistics = new JobStatistics(workspaceDirectory);
			obj = new JSONObject();
			obj.put("Number of jobs currently running", jobStatistics.getJobsRunning());
			obj.put("Number of jobs successfully completed", jobStatistics.getJobsCompleted());
			obj.put("Number of jobs completing", jobStatistics.getJobsCompleting());
			obj.put("Number of jobs failed", jobStatistics.getJobsFailed());
			obj.put("Number of jobs pending", jobStatistics.getJobsPending());
			obj.put("Number of jobs preempted", jobStatistics.getJobsPreempted());
			obj.put("Number of jobs suspended", jobStatistics.getJobsSuspended());
			obj.put("Number of jobs stopped", jobStatistics.getJobsStopped());
			obj.put("Number of jobs terminated with an error", jobStatistics.getJobsErrorTerminated());
			obj.put("Number of jobs to start", jobStatistics.getJobsNotStarted());
			obj.put("Total number of jobs submitted", jobStatistics.getJobsSubmitted());
		}
		return obj;
	}
	
	/**
	 * Produces the statistics about Slurm jobs.
	 * 
	 * @return
	 * @throws IOException
	 */
	public String getStatistics() throws IOException{
		JobStatistics jobStatistics = new JobStatistics(workspaceDirectory);
		String statistics = jobStatistics.getHTMLHeader();
		statistics = statistics + "<body>";
		statistics = statistics.concat(jobStatistics.getBodydivStart());
		statistics = statistics + "<center>";
		String headerText = "Statistics about jobs submitted to this agent are shown in the table below.<p>";
		statistics = statistics.concat(jobStatistics.getStatisticsTableHeader(headerText, "Property", "Value", "50%"));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs currently running", jobStatistics.getJobsRunning()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs successfully completed", jobStatistics.getJobsCompleted()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs completing", jobStatistics.getJobsCompleting()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs failed", jobStatistics.getJobsFailed()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs pending", jobStatistics.getJobsPending()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs preempted", jobStatistics.getJobsPreempted()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs suspended", jobStatistics.getJobsSuspended()+""));
		statistics = statistics.concat(jobStatistics.getStatisticsTableRow("Number of jobs stopped", jobStatistics.getJobsStopped()+""));
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
	 * Monitors the currently running Slurm jobs to allow new jobs to start.</br>
	 * In doing so, it checks if the number of running jobs is less than the</br>
	 * maximum number of jobs allowed to run at a time.    
	 * 
	 */
	public void monitorJobs() throws SlurmJobException{
		try {
			if(!hostAvailabilityCheck(getHpcAddress(), 22)){
				System.out.println("The agent cannot connect to the HPC server with address " + getHpcAddress());
				session = null;
				return;
			}
			scheduledIteration++;
			if (session == null || scheduledIteration%10==0) {
				if(session!=null && session.isConnected()){
					session.disconnect();
				}
				System.out.println("Initialising a session.");
				session = jsch.getSession(slurmJobProperty.getHpcServerLoginUserName(), getHpcAddress(), 22);
				String pwd = slurmJobProperty.getHpcServerLoginUserPassword();
				session.setPassword(pwd);

				try {
					// Attempt to connect to a running instance of Pageant
					Connector con = new PageantConnector();
					IdentityRepository irepo = new RemoteIdentityRepository(con);
					jsch.setIdentityRepository(irepo);
					// If successful then attempt to authenticate using a public key first,
					// falling back to using the password if no valid key is found
					session.setConfig("PreferredAuthentications", "publickey,keyboard-interactive,password");
				} catch (AgentProxyException e) {
					// Connecting to Pageant has failed so skip trying to authenticate
					// using a public key and just try with the password
					session.setConfig("PreferredAuthentications", "password");
				}

				session.setConfig("StrictHostKeyChecking", "no");
				session.connect();
				scheduledIteration = 0;
			}
			if(workspaceDirectory.isDirectory()){
				File[] jobFolders = workspaceDirectory.listFiles();
				updateRunningJobSet(jobFolders, jobsRunning);
				for(File jobFolder: jobFolders){
					if(!Utils.isJobCompleted(jobFolder, slurmJobProperty)){
						if(Utils.isJobRunning(jobFolder)){
							if(updateRunningJobsStatus(jobFolder)){
								if(jobsRunning.contains(jobFolder.getName())){
									jobsRunning.remove(jobFolder.getName());
								}
							}
						} else if(Utils.isJobNotStarted(jobFolder) && !jobsRunning.contains(jobFolder.getName())){
							if(jobsRunning.size()<slurmJobProperty.getMaxNumberOfHPCJobs()){
								try{
									boolean flag = runNotStartedJob(jobFolder);
									if(flag){
										jobsRunning.add(jobFolder.getName());
									}else{
										break;
									}
								}catch(Exception e){
									logger.info(e.getMessage());
								}
							}else{
								break;
							}
						}
					}
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			session = null;
			throw new SlurmJobException(e.getMessage());
		} catch (InterruptedException e) {
			e.printStackTrace();
			session = null;
			throw new SlurmJobException(e.getMessage());
		} catch(SftpException e){
			e.printStackTrace();
			session = null;
			throw new SlurmJobException(e.getMessage());
		} catch(JSchException e){
			e.printStackTrace();
			session = null;
			throw new SlurmJobException(e.getMessage());
		}
	}

	/**
	 * Inserts jobs which are currently running into the list of running jobs.
	 * 
	 * @param jobFolders
	 * @param jobsRunning
	 * @throws IOException
	 */
	private void updateRunningJobSet(File[] jobFolders, Set<String> jobsRunning) throws IOException{
		for(File jobFolder: jobFolders){
			if(!(new File(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()))).exists()){
				logger.info("SlurmJobAPI: job status file is not found, so the job folder with ID "+ jobFolder.getName()+" is being moved to the failed job folder.");
				System.out.println("SlurmJobAPI: job status file is not found, so the job folder with ID "+ jobFolder.getName()+" is being moved to the failed job folder.");
				try{
					Utils.moveToFailedJobsFolder(jobFolder, slurmJobProperty);
				}catch(Exception e){
					logger.info("SlurmJobAPI: failed to move the job folder with ID "+jobFolder.getName()+" to the failed job folder.");
					System.out.println("SlurmJobAPI: failed to move the job folder with ID "+jobFolder.getName()+" to the failed job folder.");
				}
				continue;
			}
			try {
				if (Utils.isJobRunning(jobFolder)) {
					jobsRunning.add(jobFolder.getName());
				}
			} catch (Exception e) {
				logger.info("SlurmJobAPI: failed to check the status of the job with ID "+jobFolder.getName()+ " while checking if it was running.");
				System.out.println("SlurmJobAPI: failed to check the status of the job with ID "+jobFolder.getName()+ " while checking if it was running.");
			}
		}
	}
	
	/**
	 * Starts running Slurm jobs which were set up before. 
	 * 
	 * @param jobFolder
	 * @throws SftpException
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 * @throws InterruptedException
	 */
	private boolean runNotStartedJob(File jobFolder)  throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		// A counter variable to count the number of mandatory files.  
		int countNumberOfFilesSetInProperties = 0;
		// Checks if the script file (including name and extension) for the current Slurm job is provided.
		if(slurmJobProperty.getSlurmScriptFileName()==null || slurmJobProperty.getSlurmScriptFileName().isEmpty()){
			throw new IOException("SlurmJobAPI: Slurm script file name and extension are not provided.");			
		}else{
			countNumberOfFilesSetInProperties++;
		}
		// Checks if the input file name for the current Slurm job is provided. 
		if(slurmJobProperty.getInputFileName()==null || slurmJobProperty.getInputFileName().isEmpty()){
			throw new IOException("SlurmJobAPI: input file name is not provided.");
		}else{
			countNumberOfFilesSetInProperties++;
		}
		// Checks if input file extension for the current Slurm job is provided.
		if(slurmJobProperty.getInputFileExtension()==null || slurmJobProperty.getInputFileExtension().isEmpty()){
			throw new IOException("SlurmJobAPI: input file extension is not provided.");
		}
		// Checks if the JSON input file name for the current Slurm job is provided.
		if(slurmJobProperty.getJsonInputFileName()==null || slurmJobProperty.getJsonInputFileName().isEmpty()){
			throw new IOException("SlurmJobAPI: JSON input file name is not provided.");
		}else{
			countNumberOfFilesSetInProperties++;
		}
		// Checks if the JSON input file extension for the current Slurm job is provided.
		if(slurmJobProperty.getJsonFileExtension()==null || slurmJobProperty.getJsonFileExtension().isEmpty()){
			throw new IOException("SlurmJobAPI: JSON file extension is not provided.");
		}
		// Checks if the executable file (including name and extension) for the current Slurm job is provided.
		if(slurmJobProperty.getExecutableFile()!=null && !slurmJobProperty.getExecutableFile().isEmpty()){
			countNumberOfFilesSetInProperties++;
		}
		int countNumberOfFilesInJobFolder = 0;
		// Checks the availability of the following four mandatory files in the current job folder:
		// 1. Slurm script file (e.g., Slurm.sh)
		// 2. Input file (e.g., input.zip or input.com)
		// 3. JSON input file (e.g., input.json)
		// 4. Status file (e.g., status.txt)
		for(File file:jobFolder.listFiles()){
			if(file.getName().equalsIgnoreCase(slurmJobProperty.getSlurmScriptFileName())){
				countNumberOfFilesInJobFolder++;
			}
			if(file.getName().endsWith(slurmJobProperty.getInputFileExtension())){
				countNumberOfFilesInJobFolder++;
			}
			if(file.getName().equalsIgnoreCase(slurmJobProperty.getJsonInputFileName().concat(slurmJobProperty.getJsonFileExtension()))){
				countNumberOfFilesInJobFolder++;
			}
			if(file.getName().equalsIgnoreCase(Status.STATUS_FILE.getName())){
				countNumberOfFilesInJobFolder++;
			}
			if(slurmJobProperty.getExecutableFile()!=null && file.getName().equalsIgnoreCase(slurmJobProperty.getExecutableFile())){
				countNumberOfFilesInJobFolder++;
			}
		}
		try{
			// If all files set through properties are not available in a job folder, it
			// deletes the folder.
			System.out.println("countNumberOfFilesInJobFolder:"+countNumberOfFilesInJobFolder);
			System.out.println("countNumberOfFilesSetInProperties:"+countNumberOfFilesSetInProperties);
			if(!(countNumberOfFilesSetInProperties>=3 && countNumberOfFilesSetInProperties+1==countNumberOfFilesInJobFolder)){
				logger.info("SlurmJobAPI: all mandatory files are not found, so the job folder with ID "+ jobFolder.getName()+" is deleted.");
				System.out.println("SlurmJobAPI: all mandatory files are not found, so the job folder with ID "+ jobFolder.getName()+" is deleted.");
				Utils.moveToFailedJobsFolder(jobFolder, slurmJobProperty);
				return false;
			}
		}catch(Exception e){
			logger.info("SlurmJobAPI: all mandatory files are not found and an attempt to move the job folder with ID "
					+jobFolder.getName()+" to the failed job folder is not successful.");
			System.out.println("SlurmJobAPI: all mandatory files are not found and an attempt to move the job folder with ID "
					+jobFolder.getName()+" to the failed job folder is not successful.");
		}
		try{
			startJob(jobFolder.getName(), Arrays.asList(jobFolder.listFiles()));
		}catch(Exception e){
			logger.info("SlurmJobAPI: the Slurm Job with ID "+jobFolder.getName()+" could not be started.");
			System.out.println("SlurmJobAPI: the Slurm Job with ID "+jobFolder.getName()+" could not be started.");
			return false;
		}
		return true;
	}
	
	/**
	 * Starts a Slurm job.
	 * 
	 * @param job
	 * @param jobFiles
	 * @throws SftpException
	 * @throws JSchException
	 * @throws IOException
	 * @throws UnknownHostException
	 * @throws InterruptedException
	 */
	private void startJob(String job, List<File> jobFiles) throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		// On HPC, the job folder and files have the address of machine where DFT Agent runs.
		// Therefore, the HPC address is replaced with the address of Agent machine.
		job = job.replace(getHpcAddress(), Utils.getMachineAddress());
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
	private void uploadFiles(String jobFolderOnHPC, List<File> jobFiles) throws SftpException, JSchException, IOException, UnknownHostException, InterruptedException{
		String replacedInputFileName = "";
		String statusFileAbsolutePath = "";
		String jobId = "";
		for(File jobFile:jobFiles){
			if(jobFile.getAbsolutePath().endsWith(slurmJobProperty.getInputFileExtension())){
				replacedInputFileName = getInputFileNameReplaced(jobFile.getAbsolutePath());
				String inputFileNameOnHPC = jobFolderOnHPC.concat("/").concat(replacedInputFileName);
				uploadFile(jobFile.getAbsolutePath(), inputFileNameOnHPC);
				replaceFileContent(jobFolderOnHPC, inputFileNameOnHPC);
			}else if(!jobFile.getAbsolutePath().endsWith(Property.STATUS_FILE_NAME.getPropertyName())){
				if(jobFile.getAbsolutePath().endsWith(".sh")){
					Utils.translateLineEndingIntoUnix(new File(jobFile.getAbsolutePath()));
				}
				uploadFile(jobFile.getAbsolutePath(), jobFolderOnHPC);				
			}
			if(jobFile.getAbsolutePath().endsWith(Property.STATUS_FILE_NAME.getPropertyName())){
				statusFileAbsolutePath = jobFile.getAbsolutePath();
			}
		}
		jobId = runSlurmJob(jobFolderOnHPC, replacedInputFileName);
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
	private String runSlurmJob(String jobFolderOnHPC, String inputFile) throws JSchException, IOException{
		inputFile = inputFile.replace(slurmJobProperty.getInputFileExtension(), "");
		String command = "cd ".concat(jobFolderOnHPC).concat(" && ")
				.concat("sbatch --job-name=").concat(inputFile).concat(" ")
				.concat(Property.SLURM_SCRIPT_FILE_NAME.getPropertyName());
		return runSlurmJob(command);
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
				.concat("sed -i 's/").concat(getHpcAddress())
				.concat("/").concat(Utils.getMachineAddress()).concat("/g' ").concat(replacedJobFileName);
		executeCommand(command);
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
		return tokens[tokens.length-1].replace(getHpcAddress(), Utils.getMachineAddress());
	}
	
	private String createJobFolder(String job) throws JSchException, IOException{
		// Creates the "mkdir" (make directory) command to create the workspace/jobspace directory.
		String command = "mkdir /home/".concat(slurmJobProperty.getHpcServerLoginUserName()).concat("/").concat(workspaceDirectory.getName());
		// Executes the command to create the workspace/jobspace directory.
		executeCommand(command);
		// Creates the command to create the job directory.
		command = "mkdir /home/".concat(slurmJobProperty.getHpcServerLoginUserName()).concat("/").concat(workspaceDirectory.getName()).concat("/").concat(job);
		// Executes the command for creating the job directory.
		executeCommand(command);
		return "/home/".concat(slurmJobProperty.getHpcServerLoginUserName()).concat("/").concat(workspaceDirectory.getName()).concat("/").concat(job);
	}
	
	/**
	 * Updates the status of a currently running job.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws JSchException
	 * @throws SftpException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	private boolean updateRunningJobsStatus(File jobFolder)
			throws JSchException, SftpException, IOException, InterruptedException {
		boolean status = false;
		try{
			File statusFile = Utils.getStatusFile(jobFolder);
			status = updateRunningJobsStatus(jobFolder.getName(), statusFile);
		}catch(Exception e){
			logger.info("SlurmJobAPI: failed to update the status of the job with ID "+jobFolder.getName()+" while checking if it was still running.");
		}
		return status;
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
	private boolean updateRunningJobsStatus(String runningJob, File statusFile) throws JSchException, SftpException, IOException, InterruptedException{
		if(statusFile!=null){
			if(!isJobRunning(statusFile)){
				if(slurmJobProperty.getOutputFileExtension().trim().toLowerCase().equals(".log")){
					if(outputFileExist(Utils.getLogFilePathOnHPC(runningJob, slurmJobProperty.getHpcServerLoginUserName(), workspaceDirectory, getHpcAddress()))){
						try{
							downloadFile(Utils.getLogFilePathOnHPC(runningJob, slurmJobProperty.getHpcServerLoginUserName(), workspaceDirectory, getHpcAddress()), Utils.getJobOutputFilePathOnAgentPC(runningJob, workspaceDirectory, runningJob, Status.EXTENSION_LOG_FILE.getName()));
							updateStatusForErrorTermination(statusFile, Utils.getJobOutputFilePathOnAgentPC(runningJob, workspaceDirectory, runningJob, Status.EXTENSION_LOG_FILE.getName()));
						}catch(Exception e){
							Utils.modifyStatus(statusFile.getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
						}
					}else{
						Utils.modifyStatus(statusFile.getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
					}
				}else{
					if(outputFileExist(Utils.getOutputFilePathOnHPC(runningJob, slurmJobProperty.getHpcServerLoginUserName(), workspaceDirectory, getHpcAddress(), slurmJobProperty.getOutputFileName().concat(slurmJobProperty.getOutputFileExtension())))){
						try{
							downloadFile(Utils.getOutputFilePathOnHPC(runningJob, slurmJobProperty.getHpcServerLoginUserName(), workspaceDirectory, getHpcAddress(), slurmJobProperty.getOutputFileName().concat(slurmJobProperty.getOutputFileExtension())), Utils.getJobOutputFilePathOnAgentPC(runningJob, workspaceDirectory, slurmJobProperty.getOutputFileName(), slurmJobProperty.getOutputFileExtension()));
						}catch(Exception e){
							Utils.modifyStatus(statusFile.getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
						}
					}else{
						Utils.modifyStatus(statusFile.getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
					}
				}
				deleteJobOnHPC(Utils.getJobFolderPathOnHPC(runningJob, slurmJobProperty.getHpcServerLoginUserName(), workspaceDirectory, getHpcAddress()));
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Checks if the output file exists. If it does, it returns true, it returns false otherwise.
	 * 
	 * @param fileAbsoultePath
	 * @return
	 * @throws JSchException
	 * @throws IOException
	 */
	private boolean outputFileExist(String fileAbsoultePath) throws JSchException, IOException{
		String command = "[ -f "+fileAbsoultePath+" ] && echo "+Status.JOB_OUTPUT_FILE_EXIST_MESSAGE.getName();
		ArrayList<String> outputs = executeCommand(command);
		if(outputs!=null && outputs.size()>0){
			if(outputs.contains(Status.JOB_OUTPUT_FILE_EXIST_MESSAGE.getName())){
				return true;
			}
		}
		return false;
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
			Utils.modifyStatus(statusFile.getAbsolutePath(), Status.JOB_LOG_MSG_ERROR_TERMINATION.getName());
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
			boolean isJobRunning = isJobRunning(jobId , statusFile);
			if(isJobRunning){
				return true; 
			}else{
				Utils.modifyStatus(statusFile.getAbsolutePath(), Status.STATUS_JOB_COMPLETED.getName());
				return false;
			}
		}
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
	 * Indicates if a server is online.
	 * 
	 * @param server refers to the server address
	 * @param port referes to the port number
	 * @return
	 */
	public boolean hostAvailabilityCheck(String server, int port) throws IOException {
		boolean available = true;
		try (final Socket dummy = new Socket(server, port)){
		} catch (UnknownHostException | IllegalArgumentException e) {
			available = false;
		}
		return available;
	}
}
