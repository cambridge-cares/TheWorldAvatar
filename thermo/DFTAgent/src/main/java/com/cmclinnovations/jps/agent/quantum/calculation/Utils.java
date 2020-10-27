package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.jps.agent.configuration.DFTAgentProperty;
import com.cmclinnovations.jps.agent.json.parser.JSonRequestParser;

import uk.ac.cam.cares.jps.base.slurm.job.Status;

public class Utils {
	private static Logger logger = LoggerFactory.getLogger(Utils.class);	
	private static long previousTimeStamp;
	
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

	public static boolean isJobCompleted(File jobFolder) throws IOException{
		return isJobFinished(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
	}
	
	public static boolean isJobOutputProcessed(File jobFolder) throws IOException{
		return isJobOutputProcessed(jobFolder.getAbsolutePath().concat(File.separator).concat(Status.STATUS_FILE.getName()));
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
	
	/**
	 * Extracts the unique species IRI, from the input.json file, for which<br>
	 * a DFT calculation was performed. 
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static String getUniqueSpeciesIRI(File jobFolder, DFTAgentProperty dftAgentProperty) throws IOException{
		BufferedReader br = openSourceFile(jobFolder.getAbsolutePath().concat(File.separator).concat(dftAgentProperty.getJsonInputFileName()).concat(dftAgentProperty.getJsonFileExtension()));
		String fileContent = getFileContent(br);
		return JSonRequestParser.getSpeciesIRI(fileContent);
	}
	
	/**
	 * Checks if thermodata agent needs to be invoked. 
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static boolean isInvokingThermoAgentRequired(File jobFolder, DFTAgentProperty dftAgentProperty) throws IOException{
		BufferedReader br = openSourceFile(jobFolder.getAbsolutePath().concat(File.separator).concat(dftAgentProperty.getJsonInputFileName()).concat(dftAgentProperty.getJsonFileExtension()));
		String fileContent = getFileContent(br);
		return JSonRequestParser.isInvokingThermoAgentRequired(fileContent);
	}
	
	/**
	 * Checks if thermodata agent needs to be invoked. 
	 * 
	 * @param jobFolder
	 * @param slurmJobProperty
	 * @return
	 * @throws IOException
	 */
	public static boolean isApplyingThermoUpdateToMechanismRequired(File jobFolder, DFTAgentProperty dftAgentProperty) throws IOException{
		BufferedReader br = openSourceFile(jobFolder.getAbsolutePath().concat(File.separator).concat(dftAgentProperty.getJsonInputFileName()).concat(dftAgentProperty.getJsonFileExtension()));
		String fileContent = getFileContent(br);
		return JSonRequestParser.isApplyingThermoUpdateToMechanismRequired(fileContent);
	}
	
	/**
	 * Returns the content of a text formatted file (e.g. .txt and .json)<br>
	 * if the file is open using BufferedReader class.
	 * 
	 * @param br
	 * @return
	 * @throws IOException
	 */
	public static String getFileContent(BufferedReader br) throws IOException{
		String fileContent = "";
		String line; 
		while((line=br.readLine())!=null){
			fileContent = fileContent.concat(line);
		}
		br.close();
		return fileContent;
	}
}
