package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.slurm.job.Status;

public class Utils {
	private static Logger logger = LoggerFactory.getLogger(Utils.class);	
	public static long previousTimeStamp;
	
	/**
	 * Generate unique time stamp to be used in naming quantum jobs and</br>
	 * the instance of DFT Agent.
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
	 * Go to the DFT Agent's job space to retrieve the status of jobs.</br>
	 * Jobs with status running or not started yet, will be classified</br>
	 * accordingly.
	 * 
	 * @return
	 */
	public static void classifyJobs(Map<String, List<String>> jobsRunning, Map<String, List<String>> jobsNotStarted, File tasksFolder) throws IOException{
		if(tasksFolder!=null && tasksFolder.exists() && tasksFolder.isDirectory()){
			File[] jobFolders = tasksFolder.listFiles();
			for(File jobFolder:jobFolders){
				classifyJob(jobsRunning, jobsNotStarted, jobFolder);
			}
		}
	}
	
	private static void classifyJob(Map<String, List<String>> jobsRunning, Map<String, List<String>> jobsNotStarted, File jobFolder) throws IOException{
		if(jobFolder.isDirectory()){
			if(isJobRunning(jobFolder)){
				retrieveRunningJobs(jobsRunning, jobFolder);
			} else if(isJobNotStarted(jobFolder)){
				retrieveNotStartedJobs(jobsNotStarted, jobFolder);
			}
		}
	}

	public static boolean isJobCompleted(File jobFolder) throws IOException{
		return isJobFinished(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	public static boolean isJobRunning(File jobFolder) throws IOException{
		return isJobRunning(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	public static boolean isJobOutputProcessed(File jobFolder) throws IOException{
		return isJobOutputProcessed(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}

	public static boolean isJobNotStarted(File jobFolder) throws IOException{
		return isJobNotStarted(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	public static void retrieveRunningJobs(Map<String, List<String>> jobsRunning, File jobFolder) throws IOException{
			File[] individualJobFiles = jobFolder.listFiles();
			List<String> runningJobsDetails = new ArrayList<>();
			for(File individualJobFile:individualJobFiles){
				if(individualJobFile.getAbsolutePath().endsWith(Status.EXTENSION_SLURM_FILE.getName()) 
						|| individualJobFile.getAbsolutePath().endsWith(Status.EXTENSION_INPUT_FILE.getName())
						|| individualJobFile.getAbsolutePath().endsWith(Status.STATUS_FILE.getName())){
					runningJobsDetails.add(individualJobFile.getAbsolutePath());
				}
			}
			if(runningJobsDetails.size()>=2){
				jobsRunning.put(jobFolder.getName(), runningJobsDetails);
			}else{
				logger.error("All files for submitting a job are not available for the following job:"+jobFolder.getAbsolutePath());
			}
	}
	

	public static void retrieveNotStartedJobs(Map<String, List<String>> jobsNotStarted, File jobFolder) throws IOException{
		File[] individualJobFiles = jobFolder.listFiles();
		List<String> notStartedJobsDetails = new ArrayList<>();
		for(File individualJobFile:individualJobFiles){
			if(individualJobFile.getAbsolutePath().endsWith(Status.EXTENSION_SLURM_FILE.getName()) 
					|| individualJobFile.getAbsolutePath().endsWith(Status.EXTENSION_INPUT_FILE.getName())
					|| individualJobFile.getAbsolutePath().endsWith(Status.STATUS_FILE.getName())){
				notStartedJobsDetails.add(individualJobFile.getAbsolutePath());
			}
		}
		if(notStartedJobsDetails.size()>=2){
			jobsNotStarted.put(jobFolder.getName(), notStartedJobsDetails);
		}else{
			logger.error("All files for submitting a job are not available for the following job:"+jobFolder.getAbsolutePath());
		}
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
	 * Modifies the output to processed in the status file. 
	 * 
	 * @param filePath the path to the status file.
	 * @param status can be empty ("") or "processed".
	 * @throws IOException
	 */
	public static void modifyOutputStatus(String filePath, String status) throws IOException{
		List<String> fileContent = new ArrayList<>();
		BufferedReader br = openSourceFile(filePath);
		String line;
		while((line=br.readLine())!=null){
		    if (line.trim().startsWith(Status.ATTRIBUTE_JOB_OUTPUT.getName())) {
		        line = Status.ATTRIBUTE_JOB_OUTPUT.getName().concat(" ").concat(status);
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
	 * Finds the status file from a list of files related to a job.
	 *  
	 * @param filePaths paths to all files in a job folder.
	 * @return
	 */
	public static File getStatusFile(List<String> filePaths){
		for(String filePath:filePaths){
			if(filePath.toLowerCase().endsWith(Status.STATUS_FILE.getName().toLowerCase())){
				return new File(filePath);
			}
		}
		return null;
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
}
