package uk.ac.cam.cares.jps.base.slurm.job;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;

import org.apache.commons.lang.math.NumberUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.io.Files;

/**
 * All functionalities for creating the workspace, job folder, status file,<br>
 * json input file, script file and input file for Slurm jobs.
 * 
 * @author msff2
 *
 */
public class Workspace {
	private Logger logger = LoggerFactory.getLogger(Workspace.class); 
	public long previousTimeStamp = System.nanoTime();

	/**
	 * Receives both the agent class (e.g. DFTAgent) and folder (absolute path)<br>
	 * where the workspace for the agent will be created, and returns the name<br>
	 * of workspace. 
	 *  
	 * @param workspaceParentPath specific to the OS. For example,<br>
	 * - on Windows an example path will be 'C:/Users/<username>', and<br>
	 * - on Linux the path '/home/<username>'.
	 * 
	 * @param agentClass
	 * @return
	 */
	public static File getWorkspace(String workspaceParentPath, String agentClass) {
		File dir = new File(workspaceParentPath);
		for (File file : dir.listFiles()) {
			if (file.isDirectory()) {
				if (file.getName().toLowerCase().startsWith(agentClass.toLowerCase())) {
					String[] tokens = file.getName().split("_");
					if (tokens.length >= 2 && tokens[tokens.length - 1].length() > 6
							&& NumberUtils.isNumber(tokens[tokens.length - 1])) {
						return file;
					}
				}
			}
		}
		return createWorkspaceName(workspaceParentPath, agentClass);
	}
	
	private static File createWorkspaceName(String workspaceParentPath, String agentClass){
		String workspaceName = agentClass.concat("_").concat("" + System.nanoTime());
		File workspace = new File(workspaceParentPath.concat(File.separator).concat(workspaceName));
		if(workspace.mkdir()){
			return workspace;
		}
		return null;
	}
	
	private boolean isWorkspaceAvailable(String workspaceParentPath, String agentClass){
		File dir = new File(workspaceParentPath);
		if(dir!=null && dir.isDirectory()){
			return isWorkspaceAvailable(dir, agentClass);
		}
		return false;
	}
	
	protected boolean isWorkspaceAvailable(File dir, String agentClass){
		for(File file:dir.listFiles()){
			if(file.isDirectory()){
				if(file.getName().toLowerCase().startsWith(agentClass.toLowerCase())){
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Creates the JSON formatted input file in the current job folder. 
	 *  
	 * @param workspaceFolder
	 * @param jsonInputFilePath the path where the JSON formatted input<br>
	 * file will be stored.
	 * @param jsonString the string that contains the the whole content<br>
	 * of the JSON formatted input.
	 * @return success message if the file JSON file can be created, null<br>
	 * otherwise. 
	 * @throws IOException
	 */
	public String createJSONInputFile(File workspaceFolder, String jsonInputFilePath, String jsonString) throws IOException{
		BufferedWriter jsonInputFile = Utils.openBufferedWriter(jsonInputFilePath);
		if(jsonInputFile == null){
			return null;
		}
		jsonInputFile.write(jsonString);
		jsonInputFile.close();
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	/**
	 * Returns the absolute path to an input file identified by its extension.<br>
	 * For example, .com, .g09, .csv, .jar, etc.
	 * 
	 * @param jobFolder
	 * @param hpcAddress
	 * @param inputFileExtension
	 * @return
	 */
	public String getInputFilePath(File jobFolder, String hpcAddress, String inputFileExtension){
		return jobFolder.getAbsolutePath()
		.concat(File.separator)
		.concat(hpcAddress)
		.concat("_").concat(getTimeStampPart(jobFolder.getName()))
		.concat(inputFileExtension);
	}
	
	public String getInputFileExtension(File input) throws SlurmJobException{
		
		if(input.isFile()){
			
			return input.getAbsolutePath().substring(input.getAbsolutePath().lastIndexOf("."));
			
		}else{
			
			logger.error("SlurmJobAPI: The provided input file is not a file.");
			
			throw new SlurmJobException("SlurmJobAPI: The provided input file is not a file.");
		}
	}
	
	/**
	 * Creates the job folder on the host machine where the current agent runs.
	 * 
	 * @param workspacePath
	 * @param hpcAddress
	 * @param timeStamp
	 * @return
	 */
	public File createJobFolder(String workspacePath, String hpcAddress, long timeStamp){
		String jobFolder = hpcAddress.concat("_").concat("" + timeStamp);
		File workspace = new File(workspacePath.concat(File.separator).concat(jobFolder));
		if(workspace.mkdir()){
			return workspace;
		}
		return null;
	}
	
	public String createInputFile(String inputFileDestinationPath, File input) throws IOException{
		copyFile(input, new File(inputFileDestinationPath));
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	public String createStatusFile(File workspaceFolder, String statusFilePath, String hpcAddress) throws IOException{
		BufferedWriter statusFile = Utils.openBufferedWriter(statusFilePath);
		if(statusFile == null){
			return null;
		}
		statusFile.write(Status.ATTRIBUTE_JOB_STATUS.getName().concat(" "));
		statusFile.write(Status.STATUS_JOB_NOT_STARTED.getName().concat("\n"));
		statusFile.write(Status.ATTRIBUTE_JOB_ID.getName().concat("\n"));
		statusFile.write(Status.ATTRIBUTE_AGENT_ID.getName().concat(" "));
		statusFile.write(workspaceFolder.getName().concat("\n"));
		statusFile.write(Status.ATTRIBUTE_HPC_ADDRESS.getName().concat(" "));
		statusFile.write(hpcAddress.concat("\n"));
		statusFile.write(Status.ATTRIBUTE_JOB_OUTPUT.getName().concat("\n"));
		statusFile.close();
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
	}
	
	public String copyFile(String source, String destination) throws IOException{
		try{
		copyFile(new File(source),
				new File(destination));
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Extracts the time stamp part from the job folder. For example,<br>
	 * if the job folder is named login-skylake.hpc.cam.ac.uk_428109593378500,<br>
	 * the method will return 428109593378500.
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
	 * Receives the jobFolder as the input and returns the absolute path to<br>
	 * to the job status file.
	 *  
	 * @param jobFolder
	 * @return
	 */
	public String getStatusFilePath(File jobFolder){
		return jobFolder.getAbsolutePath().concat(File.separator).concat(Property.STATUS_FILE_NAME.getPropertyName());
	}
	
	/**
	 * Receives the jobFolder as the input and returns the absolute path to<br>
	 * to the JSON input file.
	 * 
	 * @param jobFolder
	 * @param jsonInputFileName
	 * @return
	 */
	public String getJSONInputFilePath(File jobFolder){
		return jobFolder.getAbsolutePath().concat(File.separator).concat(Property.JSON_INPUT_FILE_NAME.getPropertyName());
	}
	

	
	/**
	 * Copy the Slurm script to the job folder to set up the current<br>
	 * job. If the method can copy the script, it returns a job set up<br>
	 * success message. Otherwise, it returns null.  
	 *  
	 *  
	 * @param source
	 * @param destination
	 * @param slurmScriptFileName
	 * @return
	 * @throws IOException
	 */
	public String copyScriptFile(String source, String destination, String slurmScriptFileName) throws IOException{
		try{
		copyFile(new File(source),
				new File(destination.concat(File.separator)
						.concat(slurmScriptFileName)));
		return Status.JOB_SETUP_SUCCESS_MSG.getName();
		}catch(IOException e){
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Copy any file from the source path to the destination path.
	 * 
	 * @param from the absolute path to the file which will be copied.
	 * @param to the absolute path to the folder where the file will be<br>
	 * copied. If user provides absolute path including the file name,<br>
	 * probably renamed or the same as the one in the from path, the method<br>
	 * will keep the same name. 
	 * 
	 * @throws IOException
	 */
	private void copyFile(File from, File to) throws IOException{
		Files.copy(from, to);
	}
}
