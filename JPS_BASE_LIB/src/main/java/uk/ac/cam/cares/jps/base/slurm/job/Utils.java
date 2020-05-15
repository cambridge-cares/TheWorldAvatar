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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Utils {
	private static Logger logger = LoggerFactory.getLogger(Utils.class);	
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
		return isJobFinished(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	/**
	 * Checks if a job is still running.
	 * 
	 * @param jobFolder
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobRunning(File jobFolder) throws IOException{
		return isJobRunning(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
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
	 * @param statusFilePath
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobFinished(String statusFilePath) throws IOException{
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
	
	/**
	 * Check the status if a job is not started yet.
	 * 
	 * @param statusFilePath the absolute path to the status file.
	 * @return
	 * @throws IOException
	 */
	public static boolean isJobNotStarted(String statusFilePath) throws IOException{
		BufferedReader statusFile = Utils.openSourceFile(statusFilePath);
		String line;
		while((line=statusFile.readLine())!=null){
			if(line.trim().startsWith(Status.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Status.STATUS_JOB_NOT_STARTED.getName())){
					statusFile.close();
					return true;
				}
			}
		}
		statusFile.close();
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
		    logger.error("SlurmJobAPI: The host address is unknown as InetAddress.getLocalHost() threw an exception.");
		}
		return null;
	}

}
