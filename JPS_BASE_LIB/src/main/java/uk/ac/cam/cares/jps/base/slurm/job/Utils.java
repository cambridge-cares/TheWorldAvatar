package uk.ac.cam.cares.jps.base.slurm.job;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;

public class Utils{
	private static Logger LOGGER = LogManager.getLogger(Utils.class);	
	public static long previousTimeStamp;
	
	/**
	 * Generate unique time stamp to be used in naming Slurm jobs.
	 * 
	 * @return
	 */
	public static long getTimeStamp(){
		long currentTimeStamp = System.nanoTime();
		while(!(currentTimeStamp > previousTimeStamp)){
			currentTimeStamp = System.nanoTime();
		}
		previousTimeStamp = currentTimeStamp;
		return currentTimeStamp;
	}
	
	/**
	 * Creates an instance of the BufferedWriter class.
	 * 
	 * @param filePathPlusName the path plus name of the file being written
	 * @return
	 * @throws IOException
	 */
	public static BufferedWriter openBufferedWriter(String filePathPlusName) throws IOException{
		return new BufferedWriter(new FileWriter(filePathPlusName));
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openSourceFile(String filePathPlusName)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				filePathPlusName), "UTF-8"));
	}
	
	/**
	 * Check if a job is completed.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobCompleted(File jobFolder) throws IOException{
		return isJobFinished(jobFolder, jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Check if a job is completed.
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobCompleted(File jobFolder, SlurmJobProperty slurmJobProperty) throws IOException{
		boolean status = false;
		try {
			status = isJobFinished(jobFolder, jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()), slurmJobProperty);
		} catch (Exception e) {
			LOGGER.info("SlurmJobAPI: failed to check the status of the job with ID " + jobFolder.getName());
		}
		return status;
	}
	
	/**
	 * Check if a job is completed with erroneous or incomplete output.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobErroneouslyCompleted(File jobFolder) throws IOException{
		return isJobErroneouslyCompleted(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Checks if a job is still running.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobRunning(File jobFolder) throws IOException{
		boolean status = false;
		try{
			status = isJobRunning(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
		}catch(Exception e){
			LOGGER.info("SlurmJobAPI: failed to check the status of the job with ID "+jobFolder.getName());
		}
		return status;
	}
	
	/**
	 * Checks if a job is not started yet. 
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobNotStarted(File jobFolder) throws IOException{
		return isJobNotStarted(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Check the status if a job finished.
	 * 
	 * @param jobFolder the folder that contains the job.
	 * @param statusFilePath the file that contains the status of the job. 
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobFinished(File jobFolder, String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_COMPLETED.getName())){
					statusFile.close();
					return true;
				}
				if(line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}

	
	/**
	 * Check the status if a job finished.
	 * 
	 * @param jobFolder the folder that contains the job.
	 * @param statusFilePath the file that contains the status of the job.
	 * @param slurmJobProperty the variable that allows to read different<br>
	 * properties including agentClass, completed jobs folder prefix, etc.
	 * 
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobFinished(File jobFolder, String statusFilePath, SlurmJobProperty slurmJobProperty) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_COMPLETED.getName())){
					statusFile.close();
					if(isJobPostProcessed(jobFolder, statusFilePath)){
						moveToCompletedJobsFolder(jobFolder, slurmJobProperty);
					}
					return true;
				}
				if(line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())){
					statusFile.close();
					moveToFailedJobsFolder(jobFolder, slurmJobProperty);
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check the status if a job post-processed.
	 * 
	 * @param jobFolder the folder that contains the job.
	 * @param statusFilePath the file that contains the status of the job.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobPostProcessed(File jobFolder, String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())){
				if(line.contains(Status.OUTPUT_PROCESSED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check the status if a job produced erroneous or incomplete output.
	 * 
	 * @param statusFilePath
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobErroneouslyCompleted(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_ERROR_TERMINATED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	public static boolean isJobOutputProcessed(File jobFolder) throws IOException{
		return isJobOutputProcessed(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Check the status if a job is currently running.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobOutputProcessed(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())){
				if(line.contains(Status.OUTPUT_PROCESSED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	/**
	 * Check the status if a job is currently running.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobRunning(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_RUNNING.getName()) 
						|| line.contains(Status.STATUS_JOB_COMPLETING.getName()) 
						|| line.contains(Status.STATUS_JOB_PENDING.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
		return false;
	}
	
	protected static boolean isStatusFileOpen = false;
	
	/**
	 * Check the status if a job is not started yet.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobNotStarted(String statusFilePath) throws IOException{
		if(isStatusFileOpen){
			return false;
		}
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		isStatusFileOpen = true;
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_NOT_STARTED.getName())){
					statusFile.close();
					isStatusFileOpen = false;
					return true;
				}
			}
		}
		statusFile.close();
		isStatusFileOpen = false;
		return false;
	}
	
	/**
	 * Reads the job id from the status file.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static String getJobId(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().toLowerCase().startsWith(Status.ATTRIBUTE_JOB_ID.getName().toLowerCase())){
				String tokens[] = line.trim().split(":");
				if(tokens.length>=2 && tokens[1].trim().length()>0){
					statusFile.close();
					return tokens[1].trim();
				}
			}
		}
		statusFile.close();
		return null;
	}
	/**
	 * Adds the job id to the status file. 
	 * 
	 * @param filePath the path to the status file.
	 * @param jobId the job id generated following the sbatch submission. 
	 * @throws IOException
	 */
	public static void addJobId(String filePath, String jobId) throws IOException{
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null){
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())) {
		        line = Status.ATTRIBUTE_JOB_STATUS.getName().concat(" ").concat(Status.STATUS_JOB_RUNNING.getName());
		    }
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_ID.getName())) {
		        line = Status.ATTRIBUTE_JOB_ID.getName().concat(" ").concat(jobId);
		    }
		    fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for(String lineContent:fileContent){
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}
	
	/**
	 * Modifies the status of job in the status file. 
	 * 
	 * @param filePath the path to the status file.
	 * @param status can be "running" or "completed".
	 * @throws IOException
	 */
	public static void modifyStatus(String filePath, String status) throws IOException{
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null){
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())) {
		        line = Status.ATTRIBUTE_JOB_STATUS.getName().concat(" ").concat(status);
		    }
		    fileContent.add(line);
		}
		br.close();
		BufferedWriter bw = openBufferedWriter(filePath);
		for(String lineContent:fileContent){
			bw.write(lineContent.concat("\n"));
		}
		bw.flush();
		bw.close();
	}

	/**
	 * Returns the status file if the job folder is provided.
	 *  
	 * @param jobFolder path to the job folder.
	 * @return
	 */
	public static File getStatusFile(File jobFolder){
		if(jobFolder.isDirectory()){
			if((new File(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName())).isFile())){
				return new File(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
			}
		}
		return null;
	}
	
	/**
	 * Returns the log file path (absolute path) on a HPC server where the<br> 
	 * Slurm job is running.
	 * 
	 * @param runningJob
	 * @param userName
	 * @param taskSpace
	 * @param hpcAddress
	 * @return
	 * @throws UnknownHostException
	 */
	public static String getLogFilePathOnHPC(String runningJob, String userName, File taskSpace, String hpcAddress) throws UnknownHostException{
		String jobFolderOnHPC = runningJob.replace(hpcAddress, getMachineAddress());
		String logFilePath = getJobFolderPathOnHPC(runningJob, userName, taskSpace, hpcAddress).concat("/").concat(jobFolderOnHPC)
				.concat(Status.EXTENSION_LOG_FILE.getName());
		return logFilePath;
	}

	/**
	 * Returns the output file path (absolute path) on a HPC server where the<br> 
	 * Slurm job is running.
	 * 
	 * @param runningJob
	 * @param userName
	 * @param taskSpace
	 * @param hpcAddress
	 * @param outputFileNameWithExtension
	 * @return
	 * @throws UnknownHostException
	 */
	public static String getOutputFilePathOnHPC(String runningJob, String userName, File taskSpace, String hpcAddress, String outputFileNameWithExtension) throws UnknownHostException{
		String outputFilePath = getJobFolderPathOnHPC(runningJob, userName, taskSpace, hpcAddress).concat("/").concat(outputFileNameWithExtension);
		return outputFilePath;
	}
	
	/**
	 * Returns the job folder path (absolute path) on a HPC where the job is running. 
	 * 
	 * @param runningJob
	 * @param userName
	 * @param taskSpace
	 * @param hpcAddress
	 * @return
	 * @throws UnknownHostException
	 */
	public static String getJobFolderPathOnHPC(String runningJob, String userName, File taskSpace, String hpcAddress) throws UnknownHostException{
		String jobFolderOnHPC = runningJob.replace(hpcAddress, getMachineAddress());
		String jobFolderPath = "/home/".concat(userName).concat("/")
				.concat(taskSpace.getName()).concat("/")
				.concat(jobFolderOnHPC);
		return jobFolderPath;
	}
	
	/**
	 * Returns the absolute path to the output file on the machine where the Agent is running.
	 * 
	 * @param runningJob
	 * @param taskSpace
	 * @param outputFileName
	 * @param outputFileExtension
	 * @return
	 */
	public static String getJobOutputFilePathOnAgentPC(String runningJob, File taskSpace, String outputFileName, String outputFileExtension){
		return taskSpace.getAbsolutePath().concat(File.separator).concat(runningJob).concat(File.separator)
				.concat(outputFileName).concat(outputFileExtension);
	}
	
	/**
	 * Checks the log file, in the case of the Gaussian software, if it is an<br>
	 * error termination.
	 * 
	 * @param logFileOnAgentPC
	 * @return
	 * @throws IOException
	 */
	public static boolean isErrorTermination(String logFileOnAgentPC)throws IOException{
		BufferedReader logFile = openSourceFile(logFileOnAgentPC);
		String line;
		while((line=logFile.readLine())!=null){
			if(line.trim().toLowerCase().startsWith(Status.JOB_LOG_MSG_ERROR_TERMINATION.getName().toLowerCase())){
				logFile.close();
				return true;
			}
		}
		logFile.close();
		return false;
	}
	
	/**
	 * Reads the address of the machine where an instance of the agent<br>
	 * running Slurm jobs is hosted.
	 * 
	 * @return
	 * @throws UnknownHostException
	 */
	public static String getMachineAddress() throws UnknownHostException{
		try {
		   InetAddress address;
		   address = InetAddress.getLocalHost();
		   return address.toString().replace("/", "_");
		} catch (UnknownHostException e){
		    e.printStackTrace();
		    LOGGER.error("SlurmJobAPI: The host address is unknown as InetAddress.getLocalHost() threw an exception.");
		}
		return null;
	}
	
	/**
	 * Windows uses both carriage return (\r) and line feed (\n) as a line<br> 
	 * ending (\r\n). Unix uses line feed (\n) as a line ending.<br>
	 * This method translates all line endings codified with "\r\n" in a<br>
	 * file into "\n".
	 * 
	 * @param file
	 */
	public static void translateLineEndingIntoUnix(File file) throws IOException{
		File recreatedFile = new File(System.getProperty("user.home").replace("\\", "/").concat("/").concat(file.getName()));
		copyModifiedContentForUnix(file, recreatedFile);
		copyModifiedContentForUnix(recreatedFile, file);
	}
	
	/**
	 * Copies file from the source path to the destination path with the<br>
	 * line ending modified to format in Unix.
	 * 
	 * @param source
	 * @param destination
	 * @throws IOException
	 */
	private static void copyModifiedContentForUnix(File source, File destination) throws IOException{
		BufferedReader receivedFile = openSourceFile(source.getAbsolutePath());
		BufferedWriter recreatedFile = openBufferedWriter(destination.getAbsolutePath());
		String line;
		while((line=receivedFile.readLine())!=null){
				recreatedFile.write(line.concat("\n"));
		}
		recreatedFile.close();
		receivedFile.close();
	}
	
	/**
	 * Moves the provided job folder to the folder that contains completed<br>
	 * and post-processed jobs of the agent. 
	 * 
	 * @param jobFolder the folder that contains a job
	 * @param slurmJobProperty
	 */
	public static void moveToCompletedJobsFolder(File jobFolder, SlurmJobProperty slurmJobProperty) {
		try {
			File destDir = getCompletedJobsDirectory(jobFolder, slurmJobProperty);
			if(destDir!=null){
				FileUtils.copyDirectory(jobFolder, destDir);
				FileUtils.deleteDirectory(jobFolder);
			}
		} catch (IOException e) {
		    e.printStackTrace();
		}
	}
	
	/**
	 * Returns the folder where completed jobs are saved.
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static File getCompletedJobsDirectory(File jobFolder, SlurmJobProperty slurmJobProperty) throws IOException{
		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), slurmJobProperty.getAgentClass());
		String completedJobsDirectory = Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator).concat(slurmJobProperty.getAgentCompletedJobsSpacePrefix()).concat(workspace.getName()).concat(File.separator).concat(jobFolder.getName());
		File jobFolderInCompletedJobs = new File(completedJobsDirectory);
		jobFolderInCompletedJobs.mkdirs();
		if(jobFolderInCompletedJobs.exists()){
			return jobFolderInCompletedJobs;
		}
		return null;
	}
	
	/**
	 * Moves the provided job folder to the folder that contains failed<br>
	 * jobs of the agent.
	 * 
	 * @param jobFolder the folder that contains a job
	 * @param slurmJobProperty
	 */
	public static void moveToFailedJobsFolder(File jobFolder, SlurmJobProperty slurmJobProperty) {
		try {
			File destDir = getFailedJobsDirectory(jobFolder, slurmJobProperty);
			if(destDir!=null){
				FileUtils.copyDirectory(jobFolder, destDir);
				FileUtils.deleteDirectory(jobFolder);
			}
		} catch (IOException e) {
		    e.printStackTrace();
		}
	}
	
	/**
	 * Returns the folder where failed jobs are saved.
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static File getFailedJobsDirectory(File jobFolder, SlurmJobProperty slurmJobProperty) throws IOException{
		File workspace = Workspace.getWorkspace(Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName(), slurmJobProperty.getAgentClass());
		String failedJobsDirectory = Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName().concat(File.separator).concat(slurmJobProperty.getAgentFailedJobsSpacePrefix()).concat(workspace.getName()).concat(File.separator).concat(jobFolder.getName());
		File jobFolderInFailedJobs = new File(failedJobsDirectory);
		jobFolderInFailedJobs.mkdirs();
		if(jobFolderInFailedJobs.exists()){
			return jobFolderInFailedJobs;
		}
		return null;
	}
}
