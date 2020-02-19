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
	
	public static Map<String, List<String>> getRunningJobs(Map<String, List<String>> jobs) throws IOException{
		Map<String, List<String>> jobsRunning = new LinkedHashMap<>();
		for(String jobFolderPath: jobs.keySet()){
			List<String> jobFiles = jobs.get(jobFolderPath);
			for(String jobFile: jobFiles){
				if(jobFile.toLowerCase().endsWith(Jobs.STATUS_FILE.getName().toLowerCase())){
					if(isJobRunning(jobFile)){
						jobsRunning.put(jobFolderPath, jobFiles);
					}
				}
			}
		}
		return jobsRunning;
	}
	
	/**
	 * Go to the DFT Agent's job space to retrieve the status of jobs.</br>
	 * Jobs with status running or not started yet, will be sent to the</br>
	 * calling method.
	 * 
	 * @return
	 */
	public static Map<String, List<String>> getUnfinishedJobs(File tasksFolder) throws IOException{
		Map<String, List<String>> jobs = new HashMap<String, List<String>>(); 
		if(tasksFolder!=null && tasksFolder.exists() && tasksFolder.isDirectory()){
			File[] jobFolders = tasksFolder.listFiles();
			for(File jobFolder:jobFolders){
				retrieveUnfinishedJobs(jobs, jobFolder);
			}
			return jobs;
		}
		return null;
	}
	
	public static void retrieveUnfinishedJobs(Map<String, List<String>> jobs, File jobFolder) throws IOException{
		if(jobFolder.isDirectory()){
			File[] individualJobFiles = jobFolder.listFiles();
			List<String> unfinishedJobsDetails = new ArrayList<>();
			for(File individualJobFile:individualJobFiles){
				boolean finished = isJobFinished(jobFolder.getAbsolutePath().concat(File.separator).concat(Jobs.STATUS_FILE.getName()));
				if(!finished 
						&& (individualJobFile.getAbsolutePath().endsWith(Jobs.EXTENSION_SLURM_FILE.getName()) 
						|| individualJobFile.getAbsolutePath().endsWith(Jobs.EXTENSION_INPUT_FILE.getName())
						|| individualJobFile.getAbsolutePath().endsWith(Jobs.STATUS_FILE.getName()))){
					unfinishedJobsDetails.add(individualJobFile.getAbsolutePath());
				}
			}
			if(unfinishedJobsDetails.size()>=2){
				jobs.put(jobFolder.getName(), unfinishedJobsDetails);
			}else{
				logger.error("All files for submitting a job are not available for the following job:"+jobFolder.getAbsolutePath());
			}
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
			if(line.trim().startsWith(Jobs.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Jobs.STATUS_JOB_COMPLETED.getName())){
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
			if(line.trim().startsWith(Jobs.ATTRIBUTE_JOB_STATUS.getName())){
				if(line.contains(Jobs.STATUS_JOB_RUNNING.getName())){
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
			if(line.trim().toLowerCase().startsWith(Jobs.ATTRIBUTE_JOB_ID.getName().toLowerCase())){
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
	 * Modifies the status of job in the status file. 
	 * 
	 * @param filePath the path to the status file.
	 * @param status can be "running" or "completed".
	 * @throws IOException
	 */
	public static void modifyStatus(String filePath, String status) throws IOException{
		List<String> fileContent = new ArrayList<>(Files.readAllLines(Paths.get(filePath)));

		for (int i = 0; i < fileContent.size(); i++) {
		    if (fileContent.get(i).startsWith(Jobs.ATTRIBUTE_JOB_STATUS.getName())) {
		        fileContent.set(i, Jobs.ATTRIBUTE_JOB_STATUS.getName().concat(" ").concat(status));
		        break;
		    }
		}
		Files.write(Paths.get(filePath), fileContent);
	}
	
	/**
	 * Finds the status file from a list of files related to a job.
	 *  
	 * @param filePaths paths to all files in a job folder.
	 * @return
	 */
	public static File getStatusFile(List<String> filePaths){
		for(String filePath:filePaths){
			if(filePath.toLowerCase().endsWith(Jobs.STATUS_FILE.getName().toLowerCase())){
				return new File(filePath);
			}
		}
		return null;
	}
	
	/**
	 * Reads the address of the machine where an instance of DFT Agent is hosted.
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
		}
		return null;
	}

}
